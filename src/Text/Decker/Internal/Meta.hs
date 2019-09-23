{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Text.Decker.Internal.Meta
  ( toPandocMeta
  , toMustacheMeta
  , mergePandocMeta
  , mergePandocMeta'
  , readMetaData
  , lookupPandocMeta
  , lookupInt
  , lookupBool
  , lookupString
  , lookupStringList
  , lookupMetaValue
  , lookupMetaBool
  , lookupMetaString
  , lookupMetaStringList
  , lookupMetaStringMap
  , lookupMetaInt
  , pandocMeta
  , DeckerException(..)
  ) where

import Text.Decker.Internal.Exception
import Text.Decker.Writer.Markdown

import Control.Arrow
import Control.Exception
import qualified Data.HashMap.Strict as H

-- import qualified Data.List as L
import Data.List.Safe ((!!))
import qualified Data.List.Split as L
import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Data.Yaml as Y
import Prelude hiding ((!!))
import System.Directory
import System.FilePath
import System.FilePath.Glob
import qualified Text.Mustache.Types as MT
import Text.Pandoc hiding (writeMarkdown)
import Text.Pandoc.Shared
import Text.Read
import Text.Regex.TDFA

-- | Converts pandoc meta data to mustache meta data. Inlines and blocks are
-- rendered to markdown strings with default options.
toMustacheMeta :: Meta -> MT.Value
toMustacheMeta (Meta mmap) = toMustacheMeta' (MetaMap mmap)

toMustacheMeta' :: MetaValue -> MT.Value
toMustacheMeta' (MetaMap mmap) =
  MT.Object $ H.fromList $ map (T.pack *** toMustacheMeta') $ Map.toList mmap
toMustacheMeta' (MetaList a) = MT.Array $ Vec.fromList $ map toMustacheMeta' a
toMustacheMeta' (MetaBool bool) = MT.Bool bool
toMustacheMeta' (MetaString string) = MT.String $ T.pack string
toMustacheMeta' (MetaInlines inlines) =
  MT.String $ writeMarkdownText def (Pandoc (Meta Map.empty) [Plain inlines])
toMustacheMeta' (MetaBlocks blocks) =
  MT.String $ writeMarkdownText def (Pandoc (Meta Map.empty) blocks)

writeMarkdownText :: WriterOptions -> Pandoc -> T.Text
writeMarkdownText options pandoc =
  case runPure $ writeMarkdown options pandoc of
    Right text -> text
    Left err -> throw $ PandocException $ show err

-- | Simple top-level merge of two meta values. Left-biased. 
mergePandocMeta :: Meta -> Meta -> Meta
mergePandocMeta (Meta meta1) (Meta meta2) = Meta $ Map.union meta1 meta2

-- | Fine-grained recursive merge of two meta values. Left-biased. 
mergePandocMeta' :: Meta -> Meta -> Meta
mergePandocMeta' (Meta left) (Meta right) =
  case merge (MetaMap left) (MetaMap right) of
    MetaMap m -> Meta m
    _ -> throw $ InternalException "This cannot happen."
  where
    merge :: MetaValue -> MetaValue -> MetaValue
    merge (MetaMap mapL) (MetaMap mapR) =
      MetaMap $ Map.unionWith merge mapL mapR
    -- merge (MetaList listL) (MetaList listR) = MetaList $ L.union listL listR
    merge left right = left

-- | Converts YAML meta data to pandoc meta data.
toPandocMeta :: Y.Value -> Meta
toPandocMeta v@(Y.Object m) =
  case toPandocMeta' v of
    (MetaMap map) -> Meta map
    _ -> Meta M.empty
toPandocMeta _ = Meta M.empty

toPandocMeta' :: Y.Value -> MetaValue
toPandocMeta' (Y.Object m) =
  MetaMap $ Map.fromList $ map (T.unpack *** toPandocMeta') $ H.toList m
toPandocMeta' (Y.Array vector) =
  MetaList $ map toPandocMeta' $ Vec.toList vector
toPandocMeta' (Y.String text) = MetaString $ T.unpack text
toPandocMeta' (Y.Number scientific) = MetaString $ show scientific
toPandocMeta' (Y.Bool bool) = MetaBool bool
toPandocMeta' Y.Null = MetaList []

decodeYaml :: FilePath -> IO Y.Value
decodeYaml yamlFile = do
  result <- Y.decodeFileEither yamlFile
  case result of
    Right object@(Y.Object _) -> return object
    Right _ ->
      throw $
      YamlException $ "Top-level meta value must be an object: " ++ yamlFile
    Left exception -> throw exception

readMetaData :: FilePath -> IO Meta
readMetaData dir = do
  let file = dir </> "decker.yaml"
  exists <- doesFileExist file
  meta <-
    if exists
      then decodeYaml file
      else return (Y.object [])
  return $ toPandocMeta meta

lookupPandocMeta :: String -> Meta -> Maybe String
lookupPandocMeta key (Meta m) =
  case L.splitOn "." key of
    [] -> Nothing
    k:ks -> lookup' ks (Map.lookup k m)
  where
    lookup' :: [String] -> Maybe MetaValue -> Maybe String
    lookup' (k:ks) (Just (MetaMap m)) = lookup' ks (Map.lookup k m)
    lookup' [] (Just (MetaBool b)) = Just $ show b
    lookup' [] (Just (MetaString s)) = Just s
    lookup' [] (Just (MetaInlines i)) = Just $ stringify i
    lookup' _ _ = Nothing

lookupInt :: String -> Int -> Meta -> Int
lookupInt key def meta =
  case lookupMetaValue key meta of
    Just (MetaString string) -> fromMaybe def $ readMaybe string
    _ -> def

lookupBool :: String -> Bool -> Meta -> Bool
lookupBool key def meta =
  case lookupMetaValue key meta of
    Just (MetaBool bool) -> bool
    _ -> def

lookupString :: String -> String -> Meta -> String
lookupString key def meta =
  case lookupMetaValue key meta of
    Just (MetaString string) -> string
    Just (MetaInlines inlines) -> stringify inlines
    _ -> def

lookupStringList :: String -> [String] -> Meta -> [String]
lookupStringList key def meta =
  case lookupMetaValue key meta of
    Just (MetaList list) -> mapMaybe metaToString list
    _ -> def

-- | Split a compound meta key at the dots and separate the array indexes.
splitKey = concatMap (L.split (L.keepDelimsL (L.oneOf "["))) . L.splitOn "."

-- | Extract the bracketed array index string.
arrayIndex :: String -> Maybe String
arrayIndex key =
  listToMaybe $
  reverse (getAllTextSubmatches (key =~ ("^\\[([0-9]+)\\]$" :: String)))

-- | Recursively deconstruct a compound key and drill into the meta data hierarchy.
lookupMetaValue :: String -> Meta -> Maybe MetaValue
lookupMetaValue key meta = lookup' (splitKey key) (MetaMap (unMeta meta))
  where
    lookup' (key:path) (MetaMap map) = M.lookup key map >>= lookup' path
    lookup' (key:path) (MetaList list) =
      arrayIndex key >>= readMaybe >>= (!!) list >>= lookup' path
    lookup' (_:_) _ = Nothing
    lookup' [] mv = Just mv

-- | Lookup a boolean value in a Pandoc meta data hierarchy. The key string
-- notation is indexed subkeys separated by '.', eg. `top.list[3].value`.
lookupMetaBool :: String -> Meta -> Maybe Bool
lookupMetaBool key meta = lookupMetaValue key meta >>= metaToBool

metaToBool :: MetaValue -> Maybe Bool
metaToBool (MetaBool bool) = Just bool
metaToBool _ = Nothing

-- | Lookup a String value in a Pandoc meta data hierarchy. The key string
-- notation is indexed subkeys separated by '.', eg. `top.list[3].value`.
lookupMetaString :: String -> Meta -> Maybe String
lookupMetaString key meta = lookupMetaValue key meta >>= metaToString

lookupMetaStringList :: String -> Meta -> Maybe [String]
lookupMetaStringList key meta = lookupMetaValue key meta >>= metaToStringList

lookupMetaStringMap :: String -> Meta -> Maybe (M.Map String String)
lookupMetaStringMap key meta = lookupMetaValue key meta >>= metaToStringMap

metaToString :: MetaValue -> Maybe String
metaToString (MetaString string) = Just string
metaToString (MetaInlines inlines) = Just $ stringify inlines
metaToString _ = Nothing

metaToStringList :: MetaValue -> Maybe [String]
metaToStringList (MetaList list) = Just $ mapMaybe metaToString list
metaToStringList _ = Nothing

metaToStringMap :: MetaValue -> Maybe (M.Map String String)
metaToStringMap (MetaMap metaMap) =
  case M.foldlWithKey'
         (\a k v ->
            case metaToString v of
              Just string -> M.insert k string a
              _ -> a)
         M.empty
         metaMap of
    stringMap
      | null stringMap -> Nothing
    stringMap -> Just stringMap
metaToStringMap _ = Nothing

lookupMetaInt :: String -> Meta -> Maybe Int
lookupMetaInt key meta = lookupMetaString key meta >>= readMaybe

pandocMeta :: (String -> Meta -> Maybe a) -> Pandoc -> String -> Maybe a
pandocMeta f (Pandoc m _) = flip f m
