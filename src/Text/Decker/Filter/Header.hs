{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Header where

import Data.Maybe
import Data.Monoid
import Relude
import Text.Decker.Filter.Attrib
import Text.Decker.Filter.Local
import Text.Decker.Filter.Media
import Text.Decker.Filter.Monad
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Pandoc
import Text.Pandoc.Walk
import qualified Text.URI as URI
import Text.Decker.Internal.URI

transformHeader :: [Block] -> Filter [Block]
transformHeader = foldlM (\blocks block -> (blocks <>) <$> transformHeader' block) []

transformHeader' :: Block -> Filter [Block]
transformHeader' h1@(Header 1 headAttr inlines)
  | containsImage inlines = do
    disp <- gets dispo
    buildMediaHeader disp $ lastImage inlines
  where
    buildMediaHeader (Disposition Deck Html) (Image imgAttr alt (url, title), rest) = do
      uri <- transformUrl url ""
      runAttrOn headAttr imgAttr $
        case classifyMedia uri imgAttr of
          ImageT -> do
            injectAttribute ("data-background-image", renderUriDecode uri)
            passAttribs
              ("data-background-" <>)
              ["size", "position", "repeat", "opacity"]
            attr <- extractAttr
            return [Header 1 attr rest]
          VideoT -> do
            injectAttribute ("data-background-video", renderUriDecode uri)
            takeClasses ("data-background-video-" <>) ["loop", "muted"]
            passAttribs ("data-background-" <>) ["size", "opacity"]
            passAttribs ("data-background-video-" <>) ["loop", "muted"]
            attr <- extractAttr
            return [Header 1 attr rest]
          IframeT -> do
            injectAttribute ("data-background-iframe", renderUriDecode uri)
            takeClasses ("data-background-" <>) ["interactive"]
            passAttribs ("data-background-" <>) ["interactive"]
            attr <- extractAttr
            return [Header 1 attr rest]
          PdfT -> do
            injectAttribute ("data-background-iframe", renderUriDecode uri)
            takeClasses ("data-background-" <>) ["interactive"]
            passAttribs ("data-background-" <>) ["interactive"]
            attr <- extractAttr
            return [Header 1 attr rest]
          _ -> return [h1]
    buildMediaHeader (Disposition _ Html) (Image imgAttr alt (url, title), rest) = do
      uri <- URI.mkURI url
      runAttrOn headAttr imgAttr $ do
        attr <- extractAttr
        imageBlock <- imageBlock uri title alt
        return [Header 1 attr rest, imageBlock]
    buildMediaHeader _ _ =
      bug $ InternalException "transformHeader: no last image in header"
-- Header does not contain any images.
transformHeader' h1@(Header 1 headAttr inlines) = do
  runAttr headAttr $ do
    ifClass ["inverse", "has-dark-background"] $ do
      injectAttribute ("data-background-color", "var(--foreground-color)")
    passAttribs ("data-background-" <>) ["color"]
    passAttribs ("data-" <>) ["background-color"]
    adjustAttribPaths' bgAttribs
    takeAllClasses
    takeAllAttributes
    takeId
    attr <- extractAttr
    return [Header 1 attr inlines]
-- Header is not level 1.
transformHeader' h@Header {} = return [h]
-- Block is not a header.
transformHeader' block = return [block]

-- | Returns true, if the list contains an image.
containsImage :: [Inline] -> Bool
containsImage = getAny . query check
  where
    check Image {} = Any True
    check _ = Any False

-- Returns the last image from a list of inlines, zaps all others and returns
-- the rest. Throws if there is no image.
lastImage :: [Inline] -> (Inline, [Inline])
lastImage inlines =
  (fromJust $ listToMaybe $ reverse $ query image inlines, zapImages inlines)
  where
    image i@Image {} = [i]
    image _ = []

-- | Replaces all images with nothing.
zapImages :: [Inline] -> [Inline]
zapImages = walk zap
  where
    zap Image {} = Str ""
    zap inline = inline
