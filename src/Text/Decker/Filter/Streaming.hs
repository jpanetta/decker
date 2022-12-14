{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Streaming where

import qualified Data.Text as Text
import Relude
import Text.Decker.Filter.Attrib
import Text.Decker.Internal.URI (setQuery)
import Text.URI (URI)
import qualified Text.URI as URI

justToList :: [Maybe a] -> [a]
justToList = reverse . justToList'
  where
    justToList' ((Just x) : xs) = x : justToList xs
    justToList' _ = []

youtubeDefaults =
  [ ("cc_load_policy", "0"),
    ("controls", "2"),
    ("iv_load_policy", "3"),
    ("modestbranding", "1"),
    ("rel", "0"),
    ("showinfo", "0")
  ]

-- https://developers.google.com/youtube/player_parameters?hl=de#IFrame_Player_API
youtubeParams =
  [ "cc_load_policy",
    "color",
    -- , "autoplay" is handled by reveal.js
    "controls",
    "disablekb",
    "enablejsapi",
    "end",
    "fs",
    "hl",
    "iv_load_policy",
    "loop",
    "modestbranding",
    "origin",
    "playsinline",
    "playlist",
    "rel",
    "showinfo",
    "start"
  ]

youtubeFlags =
  [ "cc_load_policy",
    "disablekb",
    -- , "autoplay" is handled by reveal.js
    "controls",
    "enablejsapi",
    "fs",
    "loop",
    "modestbranding",
    "playsinline",
    "rel",
    "showinfo"
  ]

-- https://dev.twitch.tv/docs/embed/video-and-clips/
twitchDefaults = [("parent", "localhost"), ("allowfullscreen", "true")]

-- https://vimeo.zendesk.com/hc/en-us/articles/360001494447-Using-Player-Parameters
vimeoDefaults =
  [ ("byline", "0"),
    ("controls", "1"),
    ("dnt", "1"),
    ("fun", "0"),
    ("title", "0"),
    ("transparent", "false")
  ]

vimeoParams =
  [ "autopause",
    -- , "autoplay" is handled by reveal.js
    "background",
    "byline",
    "color",
    "controls",
    "dnt",
    "loop",
    "muted",
    "playsinline",
    "portrait",
    "quality",
    "speed",
    "start",
    "textrack",
    "title",
    "transparent"
  ]

vimeoFlags =
  [ "autopause",
    -- , "autoplay" is handled by reveal.js
    "background",
    "byline",
    "controls",
    "dnt",
    "fun",
    "loop",
    "muted",
    "playsinline",
    "portrait",
    "speed",
    "title",
    "transparent"
  ]

mkYoutubeUri :: Text -> Attrib URI
mkYoutubeUri streamId = do
  flags <- cutClasses youtubeFlags
  params <- enableLoop flags <$> cutAttribs youtubeParams
  uri <- URI.mkURI $ "https://www.youtube.com/embed/" <> streamId
  setQuery [] (merge [params, map (,"1") flags, youtubeDefaults]) uri
  where
    enableLoop flags params =
      if "loop" `elem` flags
        then ("playlist", streamId) : params
        else params

-- Vimeo supports #t=20 for time (time is translated from start)
mkVimeoUri :: Text -> Attrib URI
mkVimeoUri streamId = do
  (_, (_, flags, params)) <- get
  params <- cutAttribs vimeoParams
  flags <- cutClasses vimeoFlags
  let start = Text.pack $ getStart params
  let params' = cleanParams params
  uri <- URI.mkURI $ "https://player.vimeo.com/video/" <> streamId <> start
  setQuery [] (merge [params', map (,"1") flags, vimeoDefaults]) uri
  where
    getStart ((x, y) : xs) =
      case x of
        "start" -> "#t=" ++ Text.unpack y
        _ -> getStart xs
    getStart [] = ""
    cleanParams (p@(x, y) : ps) =
      case x of
        "start" -> ps
        _ -> p : cleanParams ps
    cleanParams [] = []

-- Twitch supports autoplay, muted and time (time is translated from start)
-- Twitch needs autoplay="false" in URI or the video will autoplay
mkTwitchUri :: Text -> Attrib URI
mkTwitchUri streamId = do
  uri <- URI.mkURI "https://player.twitch.tv/"
  (_, (_, flags, params)) <- get
  let updatedFlags =
        ([("autoplay", "false") | "autoplay" `notElem` flags])
          ++ ([("muted", "true") | "muted" `elem` flags])
  setQuery [] ([("video", streamId)] ++ twitchDefaults ++ updatedFlags ++ getStart params) uri
  where
    getStart ((x, y) : xs) =
      case x of
        "start" -> [("time", y)]
        _ -> getStart xs
    getStart [] = []
