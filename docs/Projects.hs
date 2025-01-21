{-# LANGUAGE OverloadedStrings #-}

module Projects where

import Data.Text
import Lucid
import Lucid.Html5

renderPageToFile =
  renderToFile "Projects.html" $ do
    div_ [class_ "tile is-ancestor"] $ do
      div_ [class_ "tile is-parent is-6"] cardFinsm
      div_ [class_ "tile is-parent is-6"] cardNYH
    div_ [class_ "tile is-ancestor"] $ do
      div_ [class_ "tile is-parent is-6"] cardCCW
      div_ [class_ "tile is-parent"] cardHP0
    br_ []

createCard :: Monad m => Text -> Text -> Text -> Text -> HtmlT m ()
createCard title imageLink projLink body = do
  a_ [href_ projLink] $ do
    article_ [class_ "tile is-child box"] $ do
      figure_ [class_ "image is-4by3"] $ do
        img_ [src_ imageLink]
      br_ []
      p_ [class_ "title"] $ toHtml title
      div_ [class_ "content"] $ toHtml body

createSection :: Monad m => Text -> Text -> Text -> HtmlT m ()
createSection title imageLink body = do
  section_ [id_ title, class_ "section"] $ do
    h2_ [class_ "title is-3"] $ do
      p_ [class_ "card-header-title is-primary"] $ toHtml title
    p_ [] $ toHtml body

cardFinsm :: Monad m => HtmlT m ()
cardFinsm =
  createCard "finsm" "images/finsm.png" "https://finsm.io"$
  "A lightweight app made with Elm and the GraphicsSVG library to build, simulate and export finite state machines. Successfully used in McMaster's second year Finite Automata course by students to submit assignments. finsm.io was developed as an open-source project in conjunction with another McMaster student."

cardNYH :: Monad m => HtmlT m ()
cardNYH =
  createCard "NewYouthApp" "images/nyh.png" "https://github.com/CSchank/petri-app-land" $
  "Developed with McMaster Outreach/McMaster Start Coding students, this app was built from the ground up, beginning from the web framework, Petri App Land. Using Elm and Haskell, we worked with Brampton Multicultural Centre to provide an application for new youths to provide resources and help for integrating into Canada."

cardCCW :: Monad m => HtmlT m ()
cardCCW =
  createCard "CalcCheckWeb" "images/ccw.png" "http://calccheck.mcmaster.ca/"$
  "A theorem prover developed by Dr. Wolfram Kahl, used by second-year students in the Discrete Mathematics course. I contributed some front-end improvements and suggestions for the web interface."

cardHP0 :: Monad m => HtmlT m ()
cardHP0 =
  createCard "HP0" "images/hp0.png" "https://github.com/Necried/HP0-Front" $
  "A compiler implemented in Haskell that compiles a modified subset of Pascal code into Webassembly, then interpreted in Wasmer. The original implementation P0 was introduced in the McMaster course \"Syntax-Based Tools and Compilers\", CS 4TB3"
