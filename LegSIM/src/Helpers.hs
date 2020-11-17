{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Helpers where

import           Control.Lens
import           Data.Maybe
import           Miso
import           Miso.String

class AddSpace a where
    (<+>) :: a -> a -> a -- space

    (<./>) :: a -> a -> a -- space and comma

instance AddSpace MisoString where
    a <+> b = a <> " " <> b

    a <./> b = a <> ", " <> b

instance AddSpace String where
    a <+> b = a ++ " " ++ b

    a <./> b = a ++ ", " ++ b

showm :: Show a => a -> MisoString
showm = toMisoString . show

data Navbar action
    = Navbar { _navbarBurger :: Maybe [ View action ], _navbarStart
          :: Maybe [ View action ], _navbarEnd :: Maybe [ View action ] }

makeLenses ''Navbar

data TextColour = TextWhite | TextInfo | TextBlack | TextDanger
    deriving ( Eq )

data Size = IsSmall | IsMedium | IsLarge
    deriving ( Eq )

instance Show TextColour where
    show TextWhite  = "has-text-white"
    show TextInfo   = "has-text-info"
    show TextBlack  = "has-text-black"
    show TextDanger = "has-text-danger"

instance Show Size where
    show IsSmall  = "is-small"
    show IsMedium = "is-medium"
    show IsLarge  = "is-large"

-- |Generate a tab bar with associated click events
mkTabs :: Int                    -- ^ The active tab, indexed by the list of options
    -> [ ( JSString, action ) ]     -- ^ The list of tabs
    -> View action            -- ^ The return view

mkTabs idx tabbars = div_ [ class_ "tabs" ] [ ul_ [] rtabs ]
  where
    rtabs = tabbars & itraversed
        %@~ (\i ( a, b ) -> li_
             ([ onClick b
              ] ++ (if i == idx then [ class_ "is-active" ] else []))
             [ a_ [] [ text a ] ])

mkNavbar :: forall action. Navbar action -> View action
mkNavbar navbar = nav_ [ class_ "navbar"
                       , textProp "role" "navigation" :: Attribute action
                       , textProp "aria-label" "main navigation"
                       ] $ catMaybes [ nBurger, nStart, nEnd ]
  where
    nBurger, nStart, nEnd :: Maybe (View action)
    nBurger = navbar ^. navbarBurger >>= pure . div_ [ class_ "navbar-brand" ]

    nStart = navbar ^. navbarStart >>= pure . div_ [ class_ "navbar-start" ]

    nEnd = navbar ^. navbarEnd >>= pure . div_ [ class_ "navbar-end" ]

mkIcon :: forall action.
    Maybe action -> Maybe Size -> TextColour -> MisoString -> View action
mkIcon mOnClick msz c s = span_
    (onClickAction <> [ class_ $ "icon" <+> showm c <+> sizeOpt ])
    [ i_ [ class_ s ] [] ]
  where
    sizeOpt :: MisoString
    sizeOpt = case msz of
        Just sz -> showm sz
        _       -> ""

    onClickAction :: [ Attribute action ]
    onClickAction = case mOnClick of
        Nothing     -> []
        Just action -> [ onClick action ]
