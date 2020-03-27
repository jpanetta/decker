{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Streaming where

import Text.Decker.Filter.Attrib
import Text.Decker.Filter.Local
import Text.Decker.Filter.Macro

import Relude
import Text.Blaze.Html
import Text.Pandoc
import Text.URI (URI)
import qualified Text.URI as URI

justToList :: [Maybe a] -> [a]
justToList = reverse . justToList'
  where
    justToList' ((Just x):xs) = x : justToList xs
    justToList' (Nothing:_) = []

youtubeDefaults =
  [ ("autoplay", "0")
  , ("cc_load_policy", "0")
  , ("controls", "2")
  , ("iv_load_policy", "3")
  , ("modestbranding", "")
  , ("rel", "0")
  , ("showinfo", "0")
  ]

-- https://developers.google.com/youtube/player_parameters?hl=de#IFrame_Player_API
youtubeParams =
  [ "autoplay"
  , "cc_load_policy"
  , "color"
  , "controls"
  , "disablekb"
  , "enablejsapi"
  , "end"
  , "fs"
  , "hl"
  , "iv_load_policy"
  , "loop"
  , "modestbranding"
  , "origin"
  , "playsinline"
  , "playlist"
  , "rel"
  , "showinfo"
  , "start"
  ]

youtubeFlags =
  [ "autoplay"
  , "cc_load_policy"
  , "disablekb"
  , "enablejsapi"
  , "fs"
  , "loop"
  , "modestbranding"
  , "playsinline"
  , "rel"
  , "showinfo"
  ]

-- https://vimeo.zendesk.com/hc/en-us/articles/360001494447-Using-Player-Parameters
vimeoDefaults =
  [ ("byline", "0")
  , ("controls", "1")
  , ("dnt", "1")
  , ("fun", "0")
  , ("title", "0")
  , ("transparent", "false")
  ]

vimeoParams =
  [ "autopause"
  , "autoplay"
  , "background"
  , "byline"
  , "color"
  , "controls"
  , "dnt"
  , "fun"
  , "loop"
  , "muted"
  , "playsinline"
  , "portrait"
  , "quality"
  , "speed"
  , "textrack"
  , "title"
  , "transparent"
  ]

vimeoFlags =
  [ "autopause"
  , "autoplay"
  , "background"
  , "byline"
  , "controls"
  , "dnt"
  , "fun"
  , "loop"
  , "muted"
  , "playsinline"
  , "portrait"
  , "speed"
  , "title"
  , "transparent"
  ]

-- TODO this is just an adapter for the old stuff
streamHtml :: URI -> [Inline] -> Attrib Html
streamHtml uri caption = do
  let scheme = uriScheme uri
  width <- srcAttribute "width"
  height <- srcAttribute "height"
  let args = maybe [] (\w -> maybe [w] (\h -> [w, h]) height) width
  attr <- src
  streamId <-
    case URI.uriAuthority uri of
      Right (URI.Authority _ host _) -> pure $ URI.unRText host
      _ -> uriPath uri
  return $
    toHtml $ embedWebVideosHtml (fromMaybe "" scheme) args attr (streamId, "")
{-
 -streamHtml :: URI -> [Inline] -> Attrib Html
 -streamHtml uri caption = do
 -  streamTag <-
 -    case uriScheme uri of
 -      "youtube" -> youtubeTag uri
 -      _ -> throwM $ ResourceException "Unsupported stream service: youtube"
 -  case caption of
 -    [] -> do
 -      injectBorder >> takeUsual
 -      mkStreamTag uri <$> extractAttr
 -    caption -> do
 -      captionHtml <- lift $ inlinesToHtml caption
 -      svgAttr <- extractAttr
 -      let imageTag = mkStreamTag uri svgAttr
 -      injectBorder >> takeUsual
 -      mkFigureTag imageTag captionHtml <$> extractAttr
 -
 -mkStreamTag :: URI -> Attr -> Html
 -mkStreamTag uri (id, cs, kvs) =
 -  H.span !? (not (Text.null id), A.id (H.toValue id)) !
 -  A.class_ (H.toValue ("decker svg" : cs)) $
 -  H.preEscapedText svg
 -}