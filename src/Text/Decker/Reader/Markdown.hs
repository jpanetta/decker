{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Text.Decker.Reader.Markdown ( readAndFilterMarkdownFile ) where

import Control.Exception
import Control.Monad
import Control.Monad.Loops

import qualified Data.List as List
import qualified Data.Text.IO as Text

import Development.Shake hiding ( Resource )
import Development.Shake.FilePath as SFP

import Relude

import System.Directory

import Text.CSL.Pandoc
import Text.Decker.Filter.Decker
import Text.Decker.Filter.Filter
import Text.Decker.Filter.IncludeCode
import Text.Decker.Filter.Macro
import Text.Decker.Filter.Quiz
import Text.Decker.Filter.ShortLink
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Internal.URI
import Text.Decker.Project.Shake
import Text.Pandoc hiding ( lookupMeta )
import Text.Pandoc.Shared ( stringify )
import Text.Pandoc.Walk

-- | Reads a Markdown file and run all the the Decker specific filters on it.
-- The path is assumed to be an absolute path in the local file system under
-- the project root directory. Throws an exception if something goes wrong
readAndFilterMarkdownFile :: Disposition -> Meta -> FilePath -> Action Pandoc
readAndFilterMarkdownFile disp globalMeta path
  = do let docBase = (takeDirectory path)
       readMarkdownFile globalMeta path
         >>= liftIO . processCites'
         >>= deckerMediaFilter globalMeta docBase docBase
         >>= processPandoc deckerPipeline docBase disp Copy

-- | Reads a Markdown file from the local file system. Local resource paths are
-- converted to absolute paths. Additional meta data is read and merged into
-- the document. Other Markdown files may be transitively included. Throws an
-- exception if something goes wrong
readMarkdownFile :: Meta -> FilePath -> Action Pandoc
readMarkdownFile globalMeta path
  = do putVerbose $ "# --> readMarkdownFile: " <> path
       let base = takeDirectory path
       parseMarkdownFile path
         >>= writeBack globalMeta path
         >>= expandMeta globalMeta base
         >>= adjustResourcePaths globalMeta base
         >>= checkVersion
         >>= includeMarkdownFiles globalMeta base

-- | Standard Pandoc + Emoji support
pandocReaderOpts :: ReaderOptions
pandocReaderOpts
  = def { readerExtensions = (enableExtension Ext_emoji) pandocExtensions }

-- | Parses a Markdown file and throws an exception if something goes wrong.
parseMarkdownFile :: FilePath -> Action Pandoc
parseMarkdownFile path
  = do markdown <- liftIO $ Text.readFile path
       case runPure (readMarkdown pandocReaderOpts markdown) of
         Right pandoc -> return pandoc
         Left errMsg -> liftIO $ throwIO $ PandocException (show errMsg)

-- | Writes a Pandoc document to a file in Markdown format. Throws an exception
-- if something goes wrong
writeBack :: Meta -> FilePath -> Pandoc -> Action Pandoc
writeBack meta path pandoc
  = do let writeBack = lookupMetaOrElse False "write-back.enable" meta
       when writeBack $ writeToMarkdownFile path pandoc
       return pandoc

-- | Reads additional meta data from files listed in `meta-data:`. In case of
-- conflict, order of encouter determines preference. Later values win.
expandMeta :: Meta -> FilePath -> Pandoc -> Action Pandoc
expandMeta globalMeta base (Pandoc docMeta content)
  = do let project = lookupMetaOrFail "decker.directories.project" globalMeta
       moreMeta <- adjustMetaPaths project base docMeta
         >>= readAdditionalMeta project base
       return $ Pandoc moreMeta content

-- | Traverses the pandoc AST and adjusts local resource paths. Paths are
-- considered in these places:
--
-- 1. Url field on Image elements
-- 2. src and data-src attributes on Image and CodeBlock elements
-- 3. data-backgound-* attributes in Header 1 elements
--
adjustResourcePaths :: Meta -> FilePath -> Pandoc -> Action Pandoc
adjustResourcePaths meta base pandoc
  = walkM adjustInline pandoc >>= walkM adjustBlock
  where
    project :: String
    project = lookupMetaOrFail "decker.directories.project" meta

    -- Adjusts the image url and all source attributes. Which source is used
    -- and how is decided by the media plugin. Calling need is the
    -- responsibility of the media plugin. 
    adjustInline :: Inline -> Action Inline
    adjustInline (Image ( id, cls, kvs ) alt ( url, title ))
      = do localUrl <- adjustUrl url
           localAttr <- adjustAttribs srcAttribs kvs
           return $ Image ( id, cls, localAttr ) alt ( localUrl, title )
    adjustInline inline = return inline

    adjustUrl :: Text -> Action Text
    adjustUrl url = liftIO $ makeAbsolutePathIfLocal project base url

    -- Adjusts code block and header attributes.
    adjustBlock :: Block -> Action Block
    adjustBlock (CodeBlock ( id, cls, kvs ) text)
      = do local <- adjustAttribs srcAttribs kvs
           return $ CodeBlock ( id, cls, local ) text
    adjustBlock (Header 1 ( id, cls, kvs ) inlines)
      = do local <- adjustAttribs bgAttribs kvs
           return $ Header 1 ( id, cls, local ) inlines
    adjustBlock block = return block

    -- Adjusts the value of one attribute.
    adjustAttrib :: ( Text, Text ) -> Action ( Text, Text )
    adjustAttrib ( k, v )
      = ( k,  ) <$> (liftIO $ makeAbsolutePathIfLocal project base v)

    -- Adjusts the values of all key value attributes that are listed in keys.
    adjustAttribs
      :: [ Text ] -> [ ( Text, Text ) ] -> Action [ ( Text, Text ) ]
    adjustAttribs keys kvs
      = do let ( paths, other ) = List.partition ((`elem` keys) . fst) kvs
           local <- mapM adjustAttrib paths
           return $ local <> other

    -- Source attributes potentially used on images and code blocks.
    srcAttribs = [ "src", "data-src" ]

    -- Source attributes used on header blocks by reveal.js.
    bgAttribs
      = [ "data-background-image"
        , "data-background-video"
        , "data-background-iframe"
        ]

checkVersion :: Pandoc -> Action Pandoc
checkVersion pandoc = return pandoc

-- | Traverses the pandoc AST and transitively embeds included Markdown files.
includeMarkdownFiles :: Meta -> FilePath -> Pandoc -> Action Pandoc
includeMarkdownFiles globalMeta docBase (Pandoc docMeta content)
  = Pandoc docMeta <$> processBlocks content
  where
    processBlocks :: [ Block ] -> Action [ Block ]
    processBlocks blcks = concat . reverse <$> foldM include [] blcks

    include :: [ [ Block ] ] -> Block -> Action [ [ Block ] ]
    include document (Para [ Link _ [ Str ":include" ] ( url, _ ) ])
      = do let project
                 = lookupMetaOrFail "decker.directories.project" globalMeta
           path <- toString
             <$> (liftIO $ makeAbsolutePathIfLocal project docBase url)
           putVerbose
             $ "# --> include: " <> toString url <> " (" <> path <> ")"
           need [ path ]
           Pandoc _ includedBlocks <- readMarkdownFile globalMeta path
           return $ includedBlocks : document
    include document block = return $ [ block ] : document

-- | Adjusts meta data values that are file pathes. TODO find a more flexible
-- strategy.
adjustMetaPaths :: FilePath -> FilePath -> Meta -> Action Meta
adjustMetaPaths project base meta
  = do adjustRuntimePaths
         [ "csl", "bibliography", "meta-data", "static-resource-dirs" ]
         meta
  where
    adjustRuntimePaths :: [ Text ] -> Meta -> Action Meta
    adjustRuntimePaths keys meta
      = foldM (\meta' key -> adjustMetaValueM toAbsolute key meta') meta keys

    toAbsolute :: MetaValue -> Action MetaValue
    toAbsolute (MetaInlines inlines)
      = toAbsolute $ MetaString $ stringify inlines
    toAbsolute (MetaString uri)
      = do local <- liftIO $ absolutePathIfLocal project base uri
           case local of
             Just local
               -> do Development.Shake.doesFileExist $ toString local
                     return (MetaString local)
             Nothing -> return (MetaString uri)
    toAbsolute (MetaList uris) = MetaList <$> traverse toAbsolute uris
    toAbsolute value = return value

-- | Check for additional meta files specified in the Meta option `meta-data`.
-- Assumes that file are specified with absolute paths.
readAdditionalMeta :: FilePath -> FilePath -> Meta -> Action Meta
readAdditionalMeta project base meta
  = do let metaFiles = lookupMetaOrElse [] "meta-data" meta :: [ String ]
       putVerbose $ "# --> readAdditionalMeta: " <> show metaFiles
       moreMeta <- traverse (readMetaData project) metaFiles
       return $ foldr mergePandocMeta' meta (reverse moreMeta)

-- | Reads a meta data file. All values that are paths to local project files
-- are made absolute. Files referenced in `meta-data` are recursively loaded
-- and merged.
readMetaData :: FilePath -> FilePath -> Action Meta
readMetaData project path
  = do need [ path ]
       putVerbose $ "# --> readMetaData: " <> path
       let base = takeDirectory path
       meta <- (liftIO $ readMetaDataFile path)
         >>= adjustMetaPaths project base
       readAdditionalMeta project base meta

-- | Runs a new style decker filter. That means
--
-- 1. Put the decker path info into the documents meta data
-- 2. Run the filter.
-- 3. Take the resource info from the document meta data and
--    a) Call need on every dependency.
--    b) Provision the resources (copy to public)
-- 4. Remove all traces of this from the meta data
--
runDeckerFilter
  :: (Pandoc -> IO Pandoc) -> FilePath -> FilePath -> Pandoc -> Action Pandoc
runDeckerFilter filter topBase docBase pandoc @ (Pandoc meta blocks)
  = do dirs <- projectDirsA
       let deckerMeta
             = setMetaValue "decker.base-dir" docBase
             $ setMetaValue "decker.top-base-dir" topBase meta
       (Pandoc resultMeta resultBlocks) <- liftIO
         $ filter (Pandoc deckerMeta blocks)
       need (lookupMetaOrElse [] "decker.filter.resources" resultMeta)
       return (Pandoc meta resultBlocks)

-- | Runs the new decker media filter.
deckerMediaFilter globalMeta topBase docBase (Pandoc docMeta blocks)
  = do let combined = mergePandocMeta' docMeta globalMeta
       runDeckerFilter
         (mediaFilter options)
         topBase
         docBase
         (Pandoc combined blocks)
  where
    options
      = def
      { writerTemplate = Nothing
      , writerHTMLMathMethod = MathJax "Handled by reveal.js in the template"
      , writerExtensions
          = (enableExtension Ext_auto_identifiers . enableExtension Ext_emoji)
            pandocExtensions
      , writerCiteMethod = Citeproc
      }

-- | The old style decker filter pipeline.
deckerPipeline
  = concatM
    [ evaluateShortLinks
    , expandDeckerMacros
                                                    -- , renderCodeBlocks
    , includeCode
                                                    -- , provisionResources
    , processSlides
    , handleQuizzes
    ]-- , processCitesWithDefault

-- | Writes a pandoc document atomically to a markdown file. 
writeToMarkdownFile :: FilePath -> Pandoc -> Action ()
writeToMarkdownFile filepath pandoc @ (Pandoc pmeta _)
  = do template <- liftIO
         (compileTemplate
            ""
            "$if(titleblock)$\n$titleblock$\n\n$endif$\n\n$body$"
          >>= handleLeftM)
       let columns = lookupMetaOrElse 80 "write-back.line-columns" pmeta
       let wrapOpt :: Text -> WrapOption
           wrapOpt "none" = WrapNone
           wrapOpt "preserve" = WrapPreserve
           wrapOpt _ = WrapAuto
       let wrap = lookupMetaOrElse "none" "write-back.line-wrap" pmeta
       let extensions
             = (disableExtension Ext_simple_tables
                . disableExtension Ext_multiline_tables
                . disableExtension Ext_grid_tables
                . disableExtension Ext_raw_html
                . enableExtension Ext_auto_identifiers)
               pandocExtensions
       let options
             = def { writerTemplate = Just template
                   , writerExtensions = extensions
                   , writerColumns = columns
                   , writerWrapText = wrapOpt wrap
                   , writerSetextHeaders = False
                   }
       markdown <- liftIO
         $ runIO (writeMarkdown options pandoc) >>= handleError
       fileContent <- liftIO $ Text.readFile filepath
       when (markdown /= fileContent)
         $ withTempFile
           (\tmp
            -> liftIO $ Text.writeFile tmp markdown >> renameFile tmp filepath)
