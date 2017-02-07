{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Hakyll

import Config
import Contexts

-------------------------------------------------------------------------------
-- Main -----------------------------------------------------------------------
-------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith hakyllConf $ do

-- General Rules ---------------------------------------------------------------
    match "templates/*" $ compile templateCompiler

    match "partials/*" $ compile templateCompiler

    -- build all the tags from all the posts
    tags <- buildTags "posts/**" (fromCapture "tag-pages/*.html")

    tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged " ++ tag
      route $ idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title
                  <> constField "style" "blog.css"
                  <> listField "posts" dateCtx (return posts)
                  <> defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/blog.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls


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
                  >>= loadAndApplyTemplate "templates/default.html" defaultContext
                  >>= relativizeUrls

-- Now pages ------------------------------------------------------------------

    match "old-now/*.md" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/now.html" defaultContext
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    create ["old-now.html"] $ do
      route idRoute
      compile $ do
        nows <- recentFirst =<< loadAll "old-now/*"
        let oldNewCtx =
              listField "posts" dateCtx (return nows)
              <> constField "title" "Old-Now"
              <> constField "style" "main.css"
              <> defaultContext
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
                listField "posts" (postCtx tags) (return posts)
                <> constField "title" "Blog"
                <> constField "style" "blog.css"
                <> field "taglist" (\_ -> renderTagList tags)
                <> defaultContext
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
      route $ setExtension "html"
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
              <> defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/projects.html" projCtx
          >>= loadAndApplyTemplate "templates/default.html" projCtx
          >>= relativizeUrls