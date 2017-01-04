{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Set as S
import           Hakyll
import           Text.Pandoc.Options


-------------------------------------------------------------------------------
-- Configuration --------------------------------------------------------------
-------------------------------------------------------------------------------
dontIgnoreHtaccess :: String -> Bool
dontIgnoreHtaccess ".htaccess" = False
dontIgnoreHtaccess path        = ignoreFile defaultConfiguration path

hakyllConf :: Configuration
hakyllConf = defaultConfiguration
             { deployCommand = "./extras/deploy.sh"
             , ignoreFile = dontIgnoreHtaccess
             }

myPandocExtensions :: S.Set Extension
myPandocExtensions = S.fromList
                     [ Ext_footnotes
                     , Ext_inline_notes
                     , Ext_pandoc_title_block
                     , Ext_table_captions
                     , Ext_implicit_figures
                     , Ext_simple_tables
                     , Ext_multiline_tables
                     , Ext_grid_tables
                     , Ext_pipe_tables
                     , Ext_citations
                     , Ext_raw_tex
                     , Ext_raw_html
                     , Ext_tex_math_dollars
                     , Ext_tex_math_single_backslash
                     , Ext_latex_macros
                     , Ext_fenced_code_blocks
                     , Ext_fenced_code_attributes
                     , Ext_backtick_code_blocks
                     , Ext_inline_code_attributes
                     , Ext_markdown_in_html_blocks
                     , Ext_escaped_line_breaks
                     , Ext_fancy_lists
                     , Ext_startnum
                     , Ext_definition_lists
                     , Ext_example_lists
                     , Ext_all_symbols_escapable
                     , Ext_intraword_underscores
                     , Ext_blank_before_blockquote
                     , Ext_blank_before_header
                     , Ext_strikeout
                     , Ext_superscript
                     , Ext_subscript
                     , Ext_auto_identifiers
                     , Ext_header_attributes
                     , Ext_implicit_header_references
                     , Ext_link_attributes
                     , Ext_line_blocks]

pandocWriterOptions :: WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions
                      { writerHTMLMathMethod = MathML Nothing
                      , writerHtml5 = True
                      , writerSectionDivs = True
                      , writerReferenceLinks = True
                      }

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = defaultHakyllReaderOptions
                      { readerExtensions = myPandocExtensions
                      }

-------------------------------------------------------------------------------
-- Main -----------------------------------------------------------------------
-------------------------------------------------------------------------------


main :: IO ()
main = hakyllWith hakyllConf $ do

-- General Rules ---------------------------------------------------------------
    match "templates/*" $ compile templateCompiler

    match "partials/*" $ compile templateCompiler

    tags <- buildTags "posts/**" (fromCapture "tags/*.html")

    -- tagsRules tags $ \tag pattern -> do
    --   let title = "Posts tagged " ++ tag
    --   route $ gsubRoute " " (const "-")
    --   compile $ do
    --     posts <- constField "posts" <$> postLst pattern "templates/tag-item.html" (postCtx tags) recentFirst
    --     makeItem ""
    --       >>= loadAndApplyTemplate "templates/tagpage.html" (posts <> constField "tag" tag <> postCtx tags)
    --       >>= loadAndApplyTemplate "templates/main.html" (posts <> constField "tag" tag <> postCtx tags)


    match "files/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "files/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "CNAME" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

-- Home -----------------------------------------------------------------------
    match "*.md" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
                  >>= loadAndApplyTemplate "templates/default.html" pageCtx
                  >>= relativizeUrls

-- Now pages ------------------------------------------------------------------

    match "old-now/*.md" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/now.html" pageCtx
          >>= loadAndApplyTemplate "templates/default.html" pageCtx
          >>= relativizeUrls

    create ["old-now.html"] $ do
      route idRoute
      compile $ do
        nows <- recentFirst =<< loadAll "old-now/*"
        let oldNewCtx =
              listField "posts" dateCtx (return nows)
              <> constField "title" "Old-Now"
              <> constField "style" "main.css"
              <> pageCtx
        makeItem ""
          >>= loadAndApplyTemplate "templates/old-now.html" oldNewCtx
          >>= loadAndApplyTemplate "templates/default.html" oldNewCtx
          >>= relativizeUrls

-- Blog -----------------------------------------------------------------------

    create ["blog.html"] $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/**"
          let blogCtx =
                listField "posts" dateCtx (return posts)
                <> constField "title" "Blog"
                <> constField "style" "blog.css"
                <> pageCtx
          makeItem ""
            >>= loadAndApplyTemplate "templates/blog.html" blogCtx
            >>= loadAndApplyTemplate "templates/default.html" blogCtx
            >>= relativizeUrls

    match "posts/**" $ do
      route $ setExtension "html"
      compile $ pandocCompilerWith pandocReaderOptions pandocWriterOptions
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
        >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
        >>= relativizeUrls


-- Projects -------------------------------------------------------------------
    match "projects/*.md" $ do
      route idRoute
      compile $ pandocCompilerWith pandocReaderOptions pandocWriterOptions
        >>= relativizeUrls

    create ["projects.html"] $ do
      route idRoute
      compile $ do
        projects <- recentFirst =<< loadAll "projects/*.md"
        let projCtx =
              listField "projects" yearCtx (return projects)
              <> constField "title" "Projects"
              <> constField "style" "projects.css"
              <> pageCtx
        makeItem ""
          >>= loadAndApplyTemplate "templates/projects.html" projCtx
          >>= loadAndApplyTemplate "templates/default.html" projCtx
          >>= relativizeUrls



-------------------------------------------------------------------------------
-- Contexts -------------------------------------------------------------------
-------------------------------------------------------------------------------

-- | Custom default Context for the site
pageCtx :: Context String
pageCtx = defaultContext

dateCtx :: Context String
dateCtx = dateField "date" "%B %e, %Y" <> pageCtx

yearCtx :: Context String
yearCtx = dateField "date" "%Y" <> pageCtx

postCtx :: Tags -> Context String
postCtx tags = tagsField "tags" tags <> dateCtx

infoContext :: Context a
infoContext = field "info" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return . fromMaybe "" $ M.lookup "info" metadata
