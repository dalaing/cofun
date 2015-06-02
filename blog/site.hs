--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mempty)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "post-content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    -- http://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html
    let rss name render' =
          create [name] $ do
              route idRoute
              compile $ do
                  let feedCtx = postCtx `mappend` bodyField "description"
                  posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "post-content"
                  render' feedConfiguration feedCtx posts

    rss "rss.xml" renderRss
    rss "atom.xml" renderAtom

    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            posts <- chronological =<< loadAllSnapshots "posts/*" "post-content"
            let
                indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            pandocCompiler
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Cofun with cofree comonads"
    , feedDescription = ""
    , feedAuthorName  = "Dave Laing"
    , feedAuthorEmail = "dave.laing.80@gmail.com"
    , feedRoot        = "http://dlaing.org/cofun"
    }
