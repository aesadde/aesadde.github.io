{-|
Module      :  Config
Description : Config functions
Copyright   : (c) Alberto, 2017
License     : MIT
Maintainer  : albertosadde@gmail.com
Stability   : experimental
-}

module Config (
  dontIgnoreHtaccess,
  hakyllConf,
  pandocWriterOptions,
  pandocReaderOptions,
  myPandocExtensions ) where

import Hakyll
import           Text.Pandoc.Options
import qualified Data.Set as S

--------------------------------------------------------------------------------
-- Hakyll config ---------------------------------------------------------------
--------------------------------------------------------------------------------

dontIgnoreHtaccess :: String -> Bool
dontIgnoreHtaccess ".htaccess" = False
dontIgnoreHtaccess path        = ignoreFile defaultConfiguration path

hakyllConf :: Configuration
hakyllConf = defaultConfiguration
             { deployCommand = "./extras/deploy.sh"
             , ignoreFile = dontIgnoreHtaccess
             }

--------------------------------------------------------------------------------
-- Pandoc Config ---------------------------------------------------------------
--------------------------------------------------------------------------------
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
