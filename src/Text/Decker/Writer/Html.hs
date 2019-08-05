{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Text.Decker.Writer.Html
  ( writeIndexLists
  , markdownToHtmlDeck
  , markdownToHtmlHandout
  , markdownToHtmlPage
  , toPandocMeta
  , DeckerException(..)
  ) where

import System.Decker.OS
import Text.Decker.Filter.Filter
import Text.Decker.Filter.Macro
import Text.Decker.Filter.Quiz
import Text.Decker.Filter.Render
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Project.Sketch
import Text.Decker.Project.Version
import Text.Decker.Reader.Markdown
import Text.Decker.Resource.Resource
import Text.Decker.Resource.Template
import Text.Decker.Server.Server
import Text.Pandoc.Lens

import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Lens ((.~), (^.), (^?), at, set)
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Dynamic
import qualified Data.HashMap.Lazy as HashMap
import Data.IORef
import Data.List as List
import Data.List.Extra as List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as T
import qualified Data.Yaml as Y
import Development.Shake
import Development.Shake.FilePath as SFP
import Network.URI
import qualified System.Directory as Dir
import System.FilePath.Glob
import Text.CSL.Pandoc
import qualified Text.Mustache as M
import qualified Text.Mustache.Types as MT
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Highlighting
import Text.Pandoc.PDF
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Printf
import Text.Read (readMaybe)

-- | Generates an index.md file with links to all generated files of interest.
writeIndexLists :: FilePath -> FilePath -> Action ()
writeIndexLists out baseUrl = do
  dirs <- projectDirsA
  ts <- targetsA
  let decks = (zip (_decks ts) (_decksPdf ts))
  let handouts = (zip (_handouts ts) (_handoutsPdf ts))
  let pages = (zip (_pages ts) (_pagesPdf ts))
  decksLinks <- mapM makeLink decks
  handoutsLinks <- mapM makeLink handouts
  pagesLinks <- mapM makeLink pages
  liftIO $
    writeFile out $
    unlines
      [ "---"
      , "title: Generated Index"
      , "subtitle: " ++ dirs ^. project
      , "---"
      , "# Slide decks"
      , unlines $ decksLinks
      , "# Handouts"
      , unlines $ handoutsLinks
      , "# Supporting Documents"
      , unlines $ pagesLinks
      ]
  where
    makeLink (html, pdf) = do
      pdfExists <- doesFileExist pdf
      if pdfExists
        then return $
             printf
               "-    [%s <i class='fab fa-html5'></i>](%s) [<i class='fas fa-file-pdf'></i>](%s)"
               (takeFileName html)
               (makeRelative baseUrl html)
               (makeRelative baseUrl pdf)
        else return $
             printf
               "-    [%s <i class='fab fa-html5'></i>](%s)"
               (takeFileName html)
               (makeRelative baseUrl html)

-- | Write Pandoc in native format right next to the output file
writeNativeWhileDebugging :: FilePath -> String -> Pandoc -> Action ()
writeNativeWhileDebugging out mod doc@(Pandoc meta body) = do
  liftIO $
    runIOQuietly (writeNative pandocWriterOpts doc) >>= handleError >>=
    T.writeFile (out -<.> mod <.> ".hs")

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlDeck :: FilePath -> FilePath -> FilePath -> Action ()
markdownToHtmlDeck markdownFile out index = do
  putCurrentDocument out
  supportDir <- _support <$> projectDirsA
  supportDirRel <- getRelativeSupportDir (takeDirectory out)
  let disp = Disposition Deck Html
  pandoc@(Pandoc meta _) <- readAndProcessMarkdown markdownFile disp
  template <- getTemplate meta disp
  templateSupportDir <- getSupportDir meta out supportDirRel
  dachdeckerUrl' <- liftIO getDachdeckerUrl
  let options =
        pandocWriterOpts
          { writerSlideLevel = Just 1
          , writerTemplate = Just template
          , writerHighlightStyle = Just pygments
          , writerHTMLMathMethod =
              MathJax
                (supportDirRel </> "node_modules" </> "mathjax" </>
                 "MathJax.js?config=TeX-AMS_HTML")
          , writerVariables =
              [ ( "revealjs-url"
                , supportDirRel </> "node_modules" </> "reveal.js")
              , ("decker-support-dir", templateSupportDir)
              , ("dachdecker-url", dachdeckerUrl')
              ]
          , writerCiteMethod = Citeproc
          }
  writeDeckIndex markdownFile index pandoc >>=
    writePandocFile "revealjs" options out
  writeNativeWhileDebugging out "filtered" pandoc

writePandocFile :: String -> WriterOptions -> FilePath -> Pandoc -> Action ()
writePandocFile fmt options out pandoc =
  liftIO $
  case getWriter fmt of
    Right (TextWriter writePandoc, _) ->
      runIOQuietly (writePandoc options pandoc) >>= handleError >>=
      B.writeFile out . E.encodeUtf8
    Right (ByteStringWriter writePandoc, _) ->
      runIOQuietly (writePandoc options pandoc) >>= handleError >>=
      LB.writeFile out
    Left e -> throw $ PandocException e

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlPage :: FilePath -> FilePath -> Action ()
markdownToHtmlPage markdownFile out = do
  putCurrentDocument out
  supportDir <- getRelativeSupportDir (takeDirectory out)
  let disp = Disposition Page Html
  pandoc@(Pandoc docMeta _) <- readAndProcessMarkdown markdownFile disp
  template <- getTemplate docMeta disp
  templateSupportDir <- getSupportDir docMeta out supportDir
  let options =
        pandocWriterOpts
          { writerTemplate = Just template
          , writerHighlightStyle = Just pygments
          , writerHTMLMathMethod =
              MathJax
                (urlPath $
                 supportDir </> "node_modules" </> "mathjax" </>
                 "MathJax.js?config=TeX-AMS_HTML")
          , writerVariables = [("decker-support-dir", templateSupportDir)]
          , writerCiteMethod = Citeproc
          , writerTableOfContents = lookupBool "show-toc" False docMeta
          , writerTOCDepth = lookupInt "toc-depth" 1 docMeta
          }
  writePandocFile "html5" options out pandoc

-- | Write a markdown file to a HTML file using the handout template.
markdownToHtmlHandout :: FilePath -> FilePath -> Action ()
markdownToHtmlHandout markdownFile out = do
  putCurrentDocument out
  supportDir <- getRelativeSupportDir (takeDirectory out)
  let disp = Disposition Handout Html
  pandoc@(Pandoc docMeta _) <- readAndProcessMarkdown markdownFile disp
  template <- getTemplate docMeta disp
  templateSupportDir <- getSupportDir docMeta out supportDir
  let options =
        pandocWriterOpts
          { writerTemplate = Just template
          , writerHighlightStyle = Just pygments
          , writerHTMLMathMethod =
              MathJax
                (urlPath $
                 supportDir </> "node_modules" </> "mathjax" </>
                 "MathJax.js?config=TeX-AMS_HTML")
          , writerVariables = [("decker-support-dir", templateSupportDir)]
          , writerCiteMethod = Citeproc
          , writerTableOfContents = lookupBool "show-toc" False docMeta
          , writerTOCDepth = lookupInt "toc-depth" 1 docMeta
          }
  writePandocFile "html5" options out pandoc