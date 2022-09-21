module Decker where

import Control.Concurrent
import Control.Exception (SomeException (SomeException), catch)
import Control.Lens ((^.))
import Control.Monad.Extra
import qualified Data.ByteString as BS
import Data.IORef ()
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.String ()
import qualified Data.Text as Text
import Data.Version
import Development.Shake
import GHC.IO.Encoding
import qualified System.Directory as Dir
import System.FilePath.Posix
import System.IO
import Text.Decker.Exam.Question
import Text.Decker.Exam.Render
import Text.Decker.Exam.Xml
import Text.Decker.Filter.Index
import Text.Decker.Internal.Caches
import Text.Decker.Internal.Common
import Text.Decker.Internal.External
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Project.Version
import Text.Decker.Resource.Resource
import Text.Decker.Writer.Html
import Text.Decker.Writer.Layout
import Text.Decker.Writer.Pdf
import Text.Groom
import Text.Pandoc hiding (lookupMeta)

main :: IO ()
main = do
  setLocaleEncoding utf8
  setProjectDirectory
  run

needSel sel = needSels [sel]

needSels sels targets = need (concatMap (targets ^.) sels)

needPublicIfExists source = do
  exists <- doesFileExist source
  when exists $ need [publicDir </> source]

serverPort = 8888

serverUrl = "http://localhost:" ++ show serverPort

indexSource = "index.md"

generatedIndexSource = transientDir </> "index.md.generated"

indexFile = publicDir </> "index.html"

run :: IO ()
run = do
  warnVersion
  runDecker deckerRules

runArgs :: [String] -> IO ()
runArgs args = do
  warnVersion
  runDeckerArgs args deckerRules

deckerRules = do
  (getGlobalMeta, getTargets, getTemplate) <- prepCaches
  want ["html"]
  addHelpSuffix "Commands:"
  addHelpSuffix "  - clean - Remove all generated files."
  addHelpSuffix "  - example - Create an example project."
  addHelpSuffix "  - serve - Start just the server."
  addHelpSuffix "  - crunch - Compress all recordings to smaller size. Takes a while and will drain your battery."
  addHelpSuffix "  - pdf - Build PDF versions of all decks (*-deck.md)."
  addHelpSuffix ""
  addHelpSuffix "For additional information see: https://go.uniwue.de/decker-wiki"
  --
  withTargetDocs "Print version information." $
    phony "version" $ do
      putWarn $
        "decker version "
          ++ deckerVersion
          ++ " (branch: "
          ++ deckerGitBranch
          ++ ", commit: "
          ++ deckerGitCommitId
          ++ ", tag: "
          ++ deckerGitVersionTag
          ++ ", build date: "
          ++ deckerBuildDate
          ++ ")"
      putWarn $ "pandoc version " ++ Text.unpack pandocVersion
      putWarn $ "pandoc-types version " ++ showVersion pandocTypesVersion
  --
  withTargetDocs "Build HTML versions of all question (*-quest.md)." $
    phony "questions" $ do
      need ["support"]
      getTargets >>= needSel questions
  --
  withTargetDocs "Build HTML versions of all decks (*-deck.md)." $
    phony "decks" $ do
      meta <- getGlobalMeta
      need ["support"]
      getTargets >>= needSel decks
  --
  withTargetDocs "Build HTML versions of all decks, pages and handouts (*-deck.md, *-page.md)." $
    phony "html" $ do
      need ["support"]
      getTargets >>= needSels [decks, pages]
  --
  phony "pdf" $ do
    need ["support"]
    getTargets >>= needSel decksPdf
  --
  withTargetDocs "Compile global search index." $
    phony "search-index" $ do
      putInfo "# compiling search index ..."
      meta <- getGlobalMeta
      targets <- getTargets
      allDecks <- mapM (calcSource "-deck.html" "-deck.md") (targets ^. decks)
      buildIndex (publicDir </> "index.json") meta allDecks
  --
  withTargetDocs "If a tree falls in a forest and no one is there to hear, does it make a sound?" $
    phony "observed" $ do
      watchChangesAndRepeat
      need ["support"]
      pages <- currentlyServedPages
      need $ map (publicDir </>) pages
  --
  priority 5 $ do
    (publicDir </> "support") <//> "*" %> \out -> do
      targets <- getTargets
      let path = fromJust $ stripPrefix (publicDir <> "/") out
      let source = (targets ^. resources) Map.! out
      putVerbose $ "# extract (" <> out <> " from " <> show source <> " : " <> path <> ")"
      needResource source path
      content <- fromJust <$> liftIO (readResource path source)
      liftIO $ BS.writeFile out content
  --
  priority 4 $ do
    publicDir <//> "*-deck.html" %> \out -> do
      src <- calcSource "-deck.html" "-deck.md" out
      meta <- getGlobalMeta
      markdownToHtml htmlDeck meta getTemplate src out
      needPublicIfExists $ replaceSuffix "-deck.md" "-recording.mp4" src
      needPublicIfExists $ replaceSuffix "-deck.md" "-annot.json" src
      needPublicIfExists $ replaceSuffix "-deck.md" "-times.json" src
      needPublicIfExists $ replaceSuffix "-deck.md" "-transcript.json" src
      needPublicIfExists $ replaceSuffix "-deck.md" "-recording.vtt" src
    --
    publicDir <//> "*-deck.pdf" %> \out -> do
      let src = replaceSuffix "-deck.pdf" "-deck.html" out
      let annot = replaceSuffix "-deck.pdf" "-annot.json" $ makeRelative publicDir out
      -- This is the right way to depend on an optional file. Just check for the
      -- files existence with the Shake function `doesFileExist`.
      exists <- doesFileExist annot
      let url = serverUrl </> makeRelative publicDir src
      need [src]
      putInfo $ "# chrome started ... (for " <> out <> ")"
      result <- liftIO $ launchChrome url out
      case result of
        Right _ -> putInfo $ "# chrome finished (for " <> out <> ")"
        Left msg -> error msg
    --
    publicDir <//> "*-handout.html" %> \out -> do
      src <- calcSource "-handout.html" "-deck.md" out
      meta <- getGlobalMeta
      markdownToHtml htmlHandout meta getTemplate src out
    --
    publicDir <//> "*-page.html" %> \out -> do
      src <- calcSource "-page.html" "-page.md" out
      meta <- getGlobalMeta
      markdownToHtml htmlPage meta getTemplate src out
    --
    publicDir <//> "*.css" %> \out -> do
      let src = makeRelative publicDir out
      putNormal $ "# copy (for " <> out <> ")"
      copyFile' src out
      whenM (liftIO $ Dir.doesFileExist (src <.> "map")) $
        copyFile' (src <.> "map") (out <.> "map")
    --
    publicDir <//> "*-quest.html" %> \out -> do
      src <- calcSource "-quest.html" "-quest.yaml" out
      meta <- getGlobalMeta
      renderQuestion meta src out
    --
    privateDir <//> "quest-catalog.html" %> \out -> do
      meta <- getGlobalMeta
      targets <- getTargets
      sources <- mapM (calcSource "-quest.html" "-quest.yaml") (targets ^. questions)
      need sources
      renderCatalog meta sources out
    --
    privateDir <//> "quest-catalog.xml" %> \out -> do
      targets <- getTargets
      sources <- mapM (calcSource "-quest.html" "-quest.yaml") (targets ^. questions)
      need sources
      questions <- liftIO $ mapM readQuestion sources
      renderXmlCatalog questions out

    phony "catalogs" $ do
      need ["private/quest-catalog.html", "private/quest-catalog.xml"]
    --
    indexFile %> \out -> do
      exists <- liftIO $ Dir.doesFileExist indexSource
      let src =
            if exists
              then indexSource
              else generatedIndexSource
      need [src]
      meta <- getGlobalMeta
      markdownToHtml htmlPage meta getTemplate src out
    --
    generatedIndexSource %> \out -> do
      targets <- getTargets
      meta <- getGlobalMeta
      writeIndexLists meta targets out (takeDirectory indexFile)
  --
  priority 3 $ do
    "**/*.css" %> \out -> do
      let src = out -<.> "scss"
      whenM (liftIO $ Dir.doesFileExist src) $ do
        need [src]
        putInfo $ "# sassc (for " <> out <> ")"
        command [] "sassc" [src, out]
    --
    "**/*.plantuml.svg" %> \out -> do
      let src = dropExtension out
      need [src]
      putInfo $ "# plantuml (for " <> out <> ")"
      plantuml [src] (Just $ src -<.> "svg")
      liftIO $ Dir.renameFile (src -<.> "svg") out
    --
    "**/*.dot.svg" %> \out -> do
      let src = dropExtension out
      need [src]
      putInfo $ "# dot (for " <> out <> ")"
      dot ["-o" ++ out, src] (Just out)
    --
    "**/*.gnuplot.svg" %> \out -> do
      let src = dropExtension out
      need [src]
      putInfo $ "# gnuplot (for " <> out <> ")"
      gnuplot ["-e", "\"set output '" ++ out ++ "'\"", src] (Just out)
    --
    "**/*.tex.svg" %> \out -> do
      let src = dropExtension out
      let pdf = src -<.> ".pdf"
      let dir = takeDirectory src
      need [src]
      pdflatex ["-output-directory", dir, src] Nothing
      pdf2svg [pdf, out] (Just out)
      liftIO (Dir.removeFile pdf `catch` (\(SomeException _) -> return ()))
  --
  -- Catch all. Just copy project/* to public/*. This nicely handles ALL
  -- resources. Just `need` them where you need them.
  priority 2 $
    publicDir <//> "//" %> \out -> do
      let src = makeRelative publicDir out
      putNormal $ "# copy (for " <> out <> ")"
      copyFile' src out
  --
  withTargetDocs "Copy static file to public dir." $
    phony "static-files" $ do
      targets <- getTargets
      need (targets ^. static)
  --
  withTargetDocs "Provide information about project parameters, sources and targets" $
    phony "info" $ do
      project <- liftIO $ Dir.canonicalizePath projectDir
      putWarn $ "\nproject directory: " ++ project
      putWarn $ "public directory: " ++ publicDir
      putWarn $ "support directory: " ++ supportDir
      meta <- getGlobalMeta
      targets <- getTargets
      resources <- liftIO $ deckerResources meta
      putWarn $ "template source: " <> show resources
      putWarn "\ntargets:\n"
      putWarn (groom targets)
      putWarn "\ntop level meta data:\n"
      putWarn (groom meta)
  --
  withTargetDocs "Check the existence of usefull external programs" $
    phony "check" $ liftIO forceCheckExternalPrograms
  -- TODO use or throw away
  withTargetDocs "Copy runtime support files to public dir." $
    phony "support" $ do
      targets <- getTargets
      need [indexFile, "static-files"]
      -- Resources and their locations are now recorded in targets
      need $ Map.keys (targets ^. resources)
  withTargetDocs "Publish the public dir to the configured destination using rsync." $
    phony "publish" $ do
      need ["support"]
      meta <- getGlobalMeta
      getTargets >>= needSels [decks, pages]
      let src = publicDir ++ "/"
      case lookupMeta "publish.rsync.destination" meta of
        Just destination -> publishWithRsync src destination meta
        _ -> do
          let host = lookupMetaOrFail "rsync-destination.host" meta
          let path = lookupMetaOrFail "rsync-destination.path" meta
          let dst = intercalate ":" [host, path]
          ssh [host, "mkdir -p", path] Nothing
          rsync [src, dst] Nothing

needIfExists :: String -> String -> String -> Action ()
needIfExists suffix also out = do
  let annotDst = replaceSuffix suffix also out
  annotSrc <- calcSource' annotDst
  exists <- liftIO $ Dir.doesFileExist annotSrc
  when exists $ need [annotDst]

publishWithRsync :: String -> String -> Meta -> Action ()
publishWithRsync source destination meta = do
  let options = lookupMetaOrElse [] "publish.rsync.options" meta :: [String]
  rsync (options <> [source, destination]) Nothing

waitForYes :: IO ()
waitForYes = do
  threadDelay 1000
  putStr "\nDecker server running. Push ENTER to terminate."
  hFlush stdout
  _ <- getLine
  putStr "Terminate server? (y/N): "
  hFlush stdout
  input <- getLine
  unless (input == "y") waitForYes
