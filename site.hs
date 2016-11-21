{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Hakyll


-- Configuration --------------------------------------------------------------
dontIgnoreHtaccess :: String -> Bool
dontIgnoreHtaccess ".htaccess" = False
dontIgnoreHtaccess path        = ignoreFile defaultConfiguration path

hakyllConf :: Configuration
hakyllConf = defaultConfiguration
             {
               deployCommand = "./extras/deploy.sh"
             , ignoreFile = dontIgnoreHtaccess
}


main :: IO ()
main = hakyllWith hakyllConf $ do

-- General Rules ---------------------------------------------------------------
    match "templates/*" $ compile templateCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "CNAME" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

-- Home -----------------------------------------------------------------------
    match "index.md" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
                  >>= loadAndApplyTemplate "templates/default.html" pageCtx
                  >>= relativizeUrls

-- Now pages ------------------------------------------------------------------
    match "now.md" $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" pageCtx
          >>= relativizeUrls

    match "old-now/*.md" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" pageCtx
          >>= relativizeUrls

    create ["old-now.html"] $ do
      route idRoute
      compile $ do
        nows <- recentFirst =<< loadAll "old-now/*"
        let oldNewCtx =
              listField "nows" postCtx (return nows)
              <> constField "title" "Old-Now"
              <> pageCtx
        makeItem ""
          >>= loadAndApplyTemplate "templates/old-now.html" oldNewCtx
          >>= loadAndApplyTemplate "templates/default.html" oldNewCtx
          >>= relativizeUrls

-- Blog -----------------------------------------------------------------------
    create ["blog.html"] $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let blogCtx =
                listField "posts" postCtx (return posts)
                <> constField "title" "Blog"
                <> pageCtx
          makeItem ""
            >>= loadAndApplyTemplate "templates/blog.html" blogCtx
            >>= loadAndApplyTemplate "templates/default.html" blogCtx
            >>= relativizeUrls

    match "posts/*.md" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

-- Projects -------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Contexts -------------------------------------------------------------------
-------------------------------------------------------------------------------

-- | Custom default Context for the site
pageCtx :: Context String
pageCtx = defaultContext

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> pageCtx

infoContext :: Context a
infoContext = field "info" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "" $ M.lookup "info" metadata
