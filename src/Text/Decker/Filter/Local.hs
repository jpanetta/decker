{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Decker.Filter.Local where

import Control.Monad.Catch

import Data.Digest.Pure.MD5
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Relude

import System.Directory

import Text.Blaze.Html
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import qualified Text.Blaze.Html.Renderer.Text as Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (Attributable)
import Text.Decker.Filter.Monad
import Text.Decker.Internal.Meta
import Text.Decker.Internal.URI
import Text.Pandoc hiding (lookupMeta)
import Text.URI (URI)
import qualified Text.URI as URI

instance ToValue [Text] where
  toValue ts = toValue $ Text.intercalate " " ts

class RawHtml a where
  rawHtml :: Text -> a
  rawHtml' :: Html -> a
  rawHtml' = rawHtml . toStrict . Text.renderHtml

instance RawHtml Inline where
  rawHtml = RawInline (Format "html")

instance RawHtml [Inline] where
  rawHtml text = [RawInline (Format "html") text]

instance RawHtml Block where
  rawHtml = RawBlock (Format "html")

instance RawHtml [Block] where
  rawHtml text = [RawBlock (Format "html") text]

-- | File-extensions that should be treated as image
imageExt = ["jpg", "jpeg", "png", "gif", "tif", "tiff", "bmp", "svg"]

videoExt = ["mp4", "mov", "ogg", "avi"]

audioExt = ["mp3", "aiff", "wav"]

iframeExt = ["html", "htm", "php"]

codeExt = ["cpp", "java", "hs", "js"]

pdfExt = ["pdf"]

svgExt = ["svg"]

renderExt = ["dot", "gnuplot", "tex"]

mviewExt = ["off", "obj", "stl", "ply", "pmp"]

streamScheme = ["youtube", "vimeo", "twitch", "veer", "veer-photo"]

data MediaT
  = ImageT
  | VideoT
  | AudioT
  | IframeT
  | CodeT
  | PdfT
  | EmbedSvgT
  | MviewT
  | RenderT
  | StreamT
  deriving (Show, Eq, Ord)

classifyMedia :: URI -> Attr -> MediaT
classifyMedia uri (_, classes, _) =
  let ext = uriPathExtension uri
      scheme = uriScheme uri
   in if | ext `maybeElem` svgExt && "embed" `elem` classes -> EmbedSvgT
         | ext `maybeElem` renderExt && "render" `elem` classes -> RenderT
         | ext `maybeElem` imageExt || "image" `elem` classes -> ImageT
         | ext `maybeElem` videoExt || "video" `elem` classes -> VideoT
         | ext `maybeElem` audioExt || "audio" `elem` classes -> AudioT
         | ext `maybeElem` iframeExt || "iframe" `elem` classes -> IframeT
         | ext `maybeElem` pdfExt || "pdf" `elem` classes -> PdfT
         | ext `maybeElem` mviewExt || "mview" `elem` classes -> MviewT
         | ext `maybeElem` codeExt || "code" `elem` classes -> CodeT
         | scheme `maybeElem` streamScheme -> StreamT
         | otherwise -> ImageT

maybeElem :: Eq a => Maybe a -> [a] -> Bool
maybeElem (Just x) xs = x `elem` xs
maybeElem Nothing _ = False

renderHtml :: RawHtml a => Html -> Filter a
renderHtml html = do
  pretty <- lookupMetaOrElse False "decker.filter.pretty" <$> gets meta
  return $
    rawHtml $
    toText $
    if pretty
      then toText $ Pretty.renderHtml html
      else fromLazy $ Text.renderHtml html

booleanAttribs =
  [ "allowfullscreen"
  , "async"
  , "autofocus"
  , "autoplay"
  , "checked"
  , "controls"
  , "default"
  , "defer"
  , "disabled"
  , "formnovalidate"
  , "hidden"
  , "inert"
  , "ismap"
  , "itemscope"
  , "loop"
  , "multiple"
  , "muted"
  , "novalidate"
  , "open"
  , "readonly"
  , "required"
  , "reversed"
  , "scoped"
  , "seamless"
  , "selected"
  , "typemustmatch"
  ]

(!*) :: Attributable h => h -> [(Text, Text)] -> h
(!*) =
  foldl' (\h (k, v) -> h ! customAttribute (H.textTag k) (handleBoolean k v))
  where
    handleBoolean k v =
      if k `elem` booleanAttribs
        then H.toValue k
        else H.toValue v

mkFigureTag :: Html -> Html -> Attr -> Html
mkFigureTag content caption (id, cs, kvs) =
  H.figure !? (not (Text.null id), A.id (H.toValue id)) !
  A.class_ (H.toValue ("decker" : cs)) !*
  kvs $ do
    content
    H.figcaption ! A.class_ "decker" $ caption

-- | Renders a list of inlines to Text.
inlinesToMarkdown :: [Inline] -> Filter Text
inlinesToMarkdown [] = return ""
inlinesToMarkdown inlines = do
  FilterState options meta <- get
  case runPure (writeMarkdown options (Pandoc nullMeta [Plain inlines])) of
    Right html -> return html
    Left err -> bug $ PandocException $ "BUG: " <> show err

-- | Renders a list of inlines to HTML.
inlinesToHtml :: [Inline] -> Filter Html
inlinesToHtml [] = return $ toHtml ("" :: Text)
inlinesToHtml inlines = blocksToHtml [Plain inlines]

-- | Renders a list of blocks to HTML.
blocksToHtml :: [Block] -> Filter Html
blocksToHtml [] = return $ toHtml ("" :: Text)
blocksToHtml blocks = do
  FilterState options meta <- get
  case runPure (writeHtml5 options (Pandoc meta blocks)) of
    Right html -> return html
    Left err -> bug $ PandocException $ "BUG: " <> show err

-- | Renders a list of blocks to Markdown.
blocksToMarkdown :: [Block] -> Filter Text
blocksToMarkdown [] = return ""
blocksToMarkdown blocks = do
  FilterState options meta <- get
  case runPure (writeMarkdown options (Pandoc meta blocks)) of
    Right html -> return html
    Left err -> bug $ PandocException $ "BUG: " <> show err

writerHtmlOptions =
  def
    { writerTemplate = Nothing
    , writerHTMLMathMethod = MathJax "Handled by reveal.js in the template"
    , writerExtensions =
        (enableExtension Ext_auto_identifiers . enableExtension Ext_emoji)
          pandocExtensions
    }

-- | Renders a list of inlines to HTML. IO version that has no access
-- to the real meta data and the real writer options.
inlinesToHtml' :: [Inline] -> Html
inlinesToHtml' [] = toHtml ("" :: Text)
inlinesToHtml' inlines = blocksToHtml' [Plain inlines]

-- | Renders a list of blocks to HTML. IO version that has no access
-- to the real meta data and the real writer options.
blocksToHtml' :: [Block] -> Html
blocksToHtml' [] = toHtml ("" :: Text)
blocksToHtml' blocks =
  case runPure (writeHtml5 writerHtmlOptions (Pandoc nullMeta blocks)) of
    Right html -> html
    Left err -> bug $ PandocException $ "BUG: " <> show err

instance H.ToMarkup Inline where
  toMarkup inline = inlinesToHtml' [inline]

instance H.ToMarkup [Inline] where
  toMarkup = inlinesToHtml'

instance H.ToMarkup Block where
  toMarkup block = blocksToHtml' [block]

instance H.ToMarkup [Block] where
  toMarkup = blocksToHtml'

readLocalUri :: URI -> Filter Text
readLocalUri uri = do
  isFile <- isFileUri uri
  if isFile
    then resolveFileUri uri >>= lift . Text.readFile
    else error $ "Cannot read from remote URL" <> URI.render uri

isFileUri :: MonadThrow m => URI -> m Bool
isFileUri uri =
  case URI.uriScheme uri of
    Just rtext
      | URI.unRText rtext `notElem` ["file", "public"] -> return False
    _ -> return True

-- | Transforms a URL and handle local and remote URLs differently.
transformUrl :: Text -> Text -> Filter URI
transformUrl url ext = do
  uri <- URI.mkURI url
  transformUri uri ext

transformUri :: URI -> Text -> Filter URI
transformUri uri ext = do
  isFile <- isFileUri uri
  if isFile
    then processLocalUri uri ext
    else processRemoteUri uri

-- | Adds a remote URL to the `decker.filter.links` list in the meta data.
processRemoteUri :: URI -> Filter URI
processRemoteUri uri = do
  modifyMeta (addMetaValue "decker.filter.links" (URI.render uri))
  return uri

-- | Applies the modification function f to the meta data in the filter
-- state.
modifyMeta :: (Meta -> Meta) -> Filter ()
modifyMeta f = modify (\s -> s {meta = f (meta s)})

processLocalUri :: URI -> Text -> Filter URI
processLocalUri uri ext = do
  base <- lookupMetaOrFail "decker.base-dir" <$> gets meta
  project <- lookupMetaOrFail "decker.directories.project" <$> gets meta
  public <- lookupMetaOrFail "decker.directories.public" <$> gets meta
  case uriScheme uri of
    Just "public" -> do
      source <- setUriPath (project <> "/" <> uriPath uri) $ setUriScheme "" uri
      targetUri base source
    otherwise -> do
      checkAbsoluteUri uri
      source <- addPathExtension ext uri
      needFile $ toString $ targetPath project public source
      targetUri base source

checkAbsoluteUri :: MonadThrow m => URI -> m ()
checkAbsoluteUri uri =
  unless (URI.isPathAbsolute uri) $
  throwM $ InternalException $ "relative path detected in URI: " <> show uri

needFile :: FilePath -> Filter ()
needFile path = modifyMeta (addMetaValue "decker.filter.resources" path)

resolveFileUri :: URI -> Filter FilePath
resolveFileUri uri = do
  let urlPath = toString $ uriPath uri
  cwd <- liftIO getCurrentDirectory
  docBase <- lookupMetaOrFail "decker.base-dir" <$> gets meta
  project <- lookupMetaOrFail "decker.directories.project" <$> gets meta
  return $ makeAbsolutePath project docBase urlPath

setMeta :: Text -> Text -> Filter ()
setMeta key value = modifyMeta (setMetaValue key (MetaString value))

getMeta :: Text -> Text -> Filter Text
getMeta key def = lookupMetaOrElse def key <$> gets meta

getMetaS :: Text -> String -> Filter String
getMetaS key def = toString <$> getMeta key (toText def)

hash9String :: String -> String
hash9String text = take 9 $ show $ md5 $ encodeUtf8 text

hash9 :: Text -> Text
hash9 text = Text.pack $ take 9 $ show $ md5 $ encodeUtf8 text

single :: a -> [a]
single x = [x]
