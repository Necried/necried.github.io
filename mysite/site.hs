--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
-- import           Text.Pandoc
import           Text.Pandoc.Definition

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Projects
--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "../docs"
  }

main :: IO ()
main = do
  renderPageToFile

  hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "mybulma/css/*" $ do
        route   idRoute
        compile compressCssCompiler

{-
    match (fromList ["projects.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions pandocMap
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
-}

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            -- >>= loadAndApplyTemplate "templates/post-template.html"    postCtx
            -- >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let postCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Posts"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "Projects.html" $ do
      route idRoute
      compile $ do
        getResourceBody
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls


    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

-- Add bulma sections and title styles to Pandoc-generated markdown
pandocMap :: Pandoc -> Pandoc
pandocMap (Pandoc meta block) = Pandoc meta (addHeaderClass block)
  where
    addHeaderClass :: [Block] -> [Block]
    addHeaderClass (Header i (ident, cls, kvMap) inline : Para txt : rest) =
      Div sectionized [Header i (ident, cls, h1Class : kvMap) inline, Para txt] : addHeaderClass rest
    addHeaderClass [] = []
    addHeaderClass (x : xs) = x : addHeaderClass xs

    h1Class = ("class", "title is-3")
    sectionized = ("", ["section"], [("class","section")])

pandocBulmaCard :: Pandoc -> Pandoc
pandocBulmaCard (Pandoc meta block) = Pandoc meta (addHeaderClass block)
  where
    addHeaderClass :: [Block] -> [Block]
    addHeaderClass (Header i (ident, cls, kvMap) inline : Para txt : rest) =
      Div sectionized [Header i (ident, cls, h1Class : kvMap) inline, Para txt] : addHeaderClass rest
    addHeaderClass (x : xs) = x : addHeaderClass xs
    addHeaderClass [] = []

    h1Class = ("class", "title is-3")
    sectionized = ("", ["section"], [("class","section")])
