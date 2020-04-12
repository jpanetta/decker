{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Decker where

import Text.Decker.Internal.Exception
import Text.Decker.Internal.External
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Project.Version
import Text.Decker.Resource.Resource
import Text.Decker.Resource.Template

import Text.Decker.Writer.Html
import Text.Decker.Writer.Pdf

-- TODO Is this still used?
--import Text.Decker.Server.Dachdecker
import Control.Concurrent
import Control.Exception
import Control.Lens ((^.))
import Control.Monad.Extra
import Data.Aeson
import Data.IORef ()
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.String ()
import qualified Data.Text as Text
import Data.Version
import Development.Shake
import Development.Shake.FilePath
import qualified System.Directory as Dir
import System.Environment.Blank
import System.IO
import Text.Groom
import qualified Text.Mustache as M ()
import Text.Pandoc
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then run
    else case head args of
           "example" -> writeExampleProject
           "tutorial" -> writeTutorialProject
           _ -> run

type ParamCache a = FilePath -> Action a

type Cache a = Action a

prepCaches ::
     ProjectDirs
  -> Rules (Cache Meta, Cache Targets, ParamCache (Template Text.Text))
prepCaches directories = do
  let deckerMetaFile = (directories ^. project) </> "decker.yaml"
  let deckerTargetsFile = (directories ^. project) </> ".decker/targets.yaml"
  getGlobalMeta <-
    ($ deckerMetaFile) <$>
    newCache
      (\file -> do
         meta <- readStaticMetaData file
         let dirs =
               Meta $
               Map.fromList
                 [ ( "decker"
                   , MetaMap $
                     Map.fromList
                       [("directories", toPandocMeta' (toJSON directories))])
                 ]
         return $ mergePandocMeta' dirs meta)
  getTargets <- ($ deckerTargetsFile) <$> newCache readTargetsFile
  getTemplate <-
    newCache
      (\path -> do
         meta <- getGlobalMeta
         readTemplate meta path)
  deckerTargetsFile %> \targetFile -> do
    alwaysRerun
    meta <- getGlobalMeta
    scanTargetsToFile meta directories targetFile
  return (getGlobalMeta, getTargets, getTemplate)

needSel sel = needSels [sel]

needSels sels targets = need (concatMap (targets ^.) sels)

run :: IO ()
run = do
  when isDevelopmentVersion $
    printf
      "WARNING: You are running a development build of decker (version: %s, branch: %s, commit: %s, tag: %s). Please be sure that you know what you're doing.\n"
      deckerVersion
      deckerGitBranch
      deckerGitCommitId
      deckerGitVersionTag
  directories <- projectDirectories
  --
  let serverPort = 8888
  let serverUrl = "http://localhost:" ++ show serverPort
  let indexSource = (directories ^. project) </> "index.md"
  let indexFile = (directories ^. public) </> "index.html"
  let cruft = ["index.md.generated", "//.decker", "//.shake", "generated", "code"]
  let pdfMsg =
        "\n# To use 'decker pdf' or 'decker pdf-decks', Google Chrome has to be installed.\n" ++
        "# Windows: Currently 'decker pdf' does not work on Windows.\n" ++
        "\tPlease add 'print: true' or 'menu: true' to your slide deck and use the print button on the title slide.\n" ++
        "# MacOS: Follow the Google Chrome installer instructions.\n" ++
        "\tGoogle Chrome.app has to be located in either /Applications/Google Chrome.app or /Users/<username>/Applications/Google Chrome.app\n" ++
        "\tAlternatively you can add 'chrome' to $PATH.\n" ++
        "# Linux: 'chrome' has to be on $PATH.\n"
  --
  runDecker $ do
    (getGlobalMeta, getTargets, getTemplate) <- prepCaches directories
    --
    want ["html"]
    --
    phony "version" $ do
      putNormal $
        "decker version " ++
        deckerVersion ++
        " (branch: " ++
        deckerGitBranch ++
        ", commit: " ++
        deckerGitCommitId ++ ", tag: " ++ deckerGitVersionTag ++ ")"
      putNormal $ "pandoc version " ++ Text.unpack pandocVersion
      putNormal $ "pandoc-types version " ++ showVersion pandocTypesVersion
    --
    phony "decks" $ do
      need ["support"]
      getTargets >>= needSel decks
    --
    phony "html" $ do
      need ["support"]
      getTargets >>= needSels [decks, pages, handouts]
    --
    phony "pdf" $ do
      putNormal pdfMsg
      need ["support"]
      getTargets >>= needSels [decksPdf, handoutsPdf, pagesPdf]
    --
    phony "pdf-decks" $ do
      putNormal pdfMsg
      need ["support"]
      getTargets >>= needSel decksPdf
    --
    phony "watch" $ do
      need ["html"]
      watchChangesAndRepeat
    --
    phony "open" $ do
      need ["html"]
      openBrowser indexFile
    --
    phony "server" $ do
      need ["watch"]
      runHttpServer serverPort directories Nothing
    --
    phony "fast" $ do
      runHttpServer serverPort directories Nothing
      pages <- currentlyServedPages
      need $ map (directories ^. public </>) pages
      watchChangesAndRepeat
    --
    phony "presentation" $ do
      runHttpServer serverPort directories Nothing
      liftIO waitForYes
    --
    priority 2 $
      "//*-deck.html" %> \out -> do
        src <- calcSource "-deck.html" "-deck.md" out
        meta <- getGlobalMeta
        markdownToHtmlDeck meta getTemplate src out
    --
    priority 2 $
      "//*-deck.pdf" %> \out -> do
        let src = replaceSuffix "-deck.pdf" "-deck.html" out
        need [src]
        putNormal $ "Started: " ++ src ++ " -> " ++ out
        runHttpServer serverPort directories Nothing
        result <-
          liftIO $
          launchChrome
            (serverUrl </> makeRelative (directories ^. public) src)
            out
        case result of
          Right msg -> putNormal msg
          Left msg -> error msg
    --
    priority 2 $
      "//*-handout.html" %> \out -> do
        src <- calcSource "-handout.html" "-deck.md" out
        meta <- getGlobalMeta
        markdownToHtmlHandout meta getTemplate src out
    --
    priority 2 $
      "//*-handout.pdf" %> \out -> do
        src <- calcSource "-handout.pdf" "-deck.md" out
        meta <- getGlobalMeta
        markdownToPdfHandout meta getTemplate src out
    --
    priority 2 $
      "//*-page.html" %> \out -> do
        src <- calcSource "-page.html" "-page.md" out
        meta <- getGlobalMeta
        markdownToHtmlPage meta getTemplate src out
    --
    priority 2 $
      "//*-page.pdf" %> \out -> do
        src <- calcSource "-page.pdf" "-page.md" out
        meta <- getGlobalMeta
        markdownToPdfPage meta getTemplate src out
    --
    priority 2 $
      indexFile %> \out -> do
        exists <- Development.Shake.doesFileExist indexSource
        let src =
              if exists
                then indexSource
                else indexSource <.> "generated"
        meta <- getGlobalMeta
        markdownToHtmlPage meta getTemplate src out
    --
    indexSource <.> "generated" %> \out -> do
      targets <- getTargets
      meta <- getGlobalMeta
      writeIndexLists meta targets out (takeDirectory indexFile)
    --
    priority 2 $
      "//*.dot.svg" %> \out -> do
        let src = dropExtension out
        need [src]
        dot ["-o" ++ out, src]
    --
    priority 2 $
      "//*.gnuplot.svg" %> \out -> do
        let src = dropExtension out
        need [src]
        gnuplot ["-e", "set output \"" ++ out ++ "\"", src]
    --
    priority 2 $
      "//*.tex.svg" %> \out -> do
        let src = dropExtension out
        let pdf = src -<.> ".pdf"
        let dir = takeDirectory src
        need [src]
        pdflatex ["-output-directory", dir, src]
        pdf2svg [pdf, out]
        liftIO $ Dir.removeFile pdf
    --
    phony "clean" $ do
      removeFilesAfter (directories ^. public) ["//"]
      removeFilesAfter (directories ^. project) cruft
    --
    -- TODO: Move help-page out of the template dir
    -- phony "help" $ do
    --  text <- getTemplate' "template/help-page.md"
    --  liftIO $ Text.putStr text
    --
    phony "info" $ do
      putNormal $ "\nproject directory: " ++ (directories ^. project)
      putNormal $ "public directory: " ++ (directories ^. public)
      putNormal $ "support directory: " ++ (directories ^. support)
      meta <- getGlobalMeta
      targets <- getTargets
      templateSource <-
        liftIO $ calcTemplateSource (getMetaText "template-source" meta)
      putNormal $ "template source: " <> show templateSource
      putNormal "\ntargets:\n"
      putNormal (groom targets)
      putNormal "\ntop level meta data:\n"
      putNormal (groom meta)
    --
    phony "support" $ do
      need [indexFile]
      meta <- getGlobalMeta
      writeSupportFilesToPublic meta
    --
    phony "check" checkExternalPrograms
    --
    phony "publish" $ do
      meta <- getGlobalMeta
      need ["support"]
      getTargets >>= needSels [decks, handouts, pages]
      let host = getMetaString "rsync-destination.host" meta
      let path = getMetaString "rsync-destination.path" meta
      if isJust host && isJust path
        then do
          let src = (directories ^. public) ++ "/"
          let dst = intercalate ":" [fromJust host, fromJust path]
          ssh [fromJust host, "mkdir -p", fromJust path]
          rsync [src, dst]
        else throw RsyncUrlException
    -- TODO Is this still needed?
    --phony "sync" $ uploadQuizzes (_sources <$> targetsA)

-- TODO Does this even make sense with all the diagnostic output?
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
