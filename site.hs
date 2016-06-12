{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid ((<>))
import           Hakyll
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "CNAME" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["now.md", "about.md","contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" (metaKeywordContext     <> defaultContext)
            >>= relativizeUrls

    match "posts/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"     (metaKeywordContext <> postCtx)
            >>= loadAndApplyTemplate "templates/default.html"  (metaKeywordContext    <> postCtx)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" (metaKeywordContext     <> archiveCtx)
                >>= loadAndApplyTemplate "templates/default.html" (metaKeywordContext     <> archiveCtx)
                >>= relativizeUrls

    --- Homepage
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Home"                <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (metaKeywordContext     <> indexCtx)
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
-- Contexts --------------------------------------------------------------------
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext
--------------------------------------------------------------------------------

-- add metadata keywords to html file
metaKeywordContext :: Context String
metaKeywordContext = field "metaKeywords" $ \item -> do
  tags <- getMetadataField (itemIdentifier item) "tags"
  return $ maybe "" showMetaTags tags
    where
      showMetaTags :: String -> String
      showMetaTags t = "<meta name=\"keywords\" content=\"" ++ t ++ "\"/>\n"
--------------------------------------------------------------------------------
