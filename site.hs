--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

--------------------------------------------------------------------------------

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration =
  FeedConfiguration
    { feedTitle = "Bitestring Blog",
      feedDescription = "Thoughts and articles related to programming, free software, web, privacy and security.",
      feedAuthorName = "Bitestring",
      feedAuthorEmail = "",
      feedRoot = "https://bitestring.com"
    }

--------------------------------------------------------------------------------

hakyllConfig :: Configuration
hakyllConfig =
  defaultConfiguration
    { destinationDirectory = "docs"
    }

--------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith hakyllConfig $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx <> bodyField "description"
        posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
        renderAtom myFeedConfiguration feedCtx posts

    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx <> bodyField "description"
        posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
        renderRss myFeedConfiguration feedCtx posts


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
