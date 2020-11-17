{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Register ( Model, Action, initialModel, Interface(..), updateModel
                , viewModel, makeSelected, makeNotSelected, registerData ) where

import           CPUComponent
import           Control.Lens (makeLenses, (&), (.=), (.~), (^.))
import           Data.Word
import           Helpers
import           Miso
import           Miso.String

data Model = Model { _registerData :: !Word64, _selectedState
                 :: SelectedState, _registerLabel :: !Int }
    deriving ( Show, Eq )

makeLenses ''Model

initialModel :: Int -> Model
initialModel label = Model 0 NotSelected label

instance CPUComponent Model where
    makeSelected m = m & selectedState .~ Selected
    makeNotSelected m = m & selectedState .~ NotSelected

      -- saveRegister :: action
data Action = SelectRegister | EnterData MisoString | SaveRegister
    deriving ( Show, Eq )

data Interface action
    = Interface {
          -- passAction :: Action -> action
          typeAction :: (MisoString -> Action) -> MisoString -> action }

updateModel :: Interface action -> Action -> Miso.Transition action Model ()
updateModel iface action = case action of
    SelectRegister -> selectedState .= Selected
    SaveRegister   -> selectedState .= NotSelected
    _              -> pure ()

viewModel :: Interface action -> Model -> Miso.View action
viewModel iface m
    = let regLabel = span_ [ class_ highlightSelected ]
              [ text $ toMisoString $ "R" <> show (m ^. registerLabel) ]
          inpAction = typeAction iface EnterData
          lvlItem = div_ [ class_ "level-item" ]
          inp = div_ [ class_ "level-left" ]
              [ lvlItem [ p_ [ class_ "subtitle is-6" ] [ regLabel ] ]
              , lvlItem [ p_ [ class_ "control" ]
                              [ input_ [ class_ "input is-small"
                                       , type_ "text"
                                       , value_ (showm $ m ^. registerData)
                                       , disabled_ True
                                       ]
                              ]
                        ]
              ]
          highlightSelected = case m ^. selectedState of
              Selected    -> "tag is-warning"
              NotSelected -> "tag is-black"
    in div_ [ class_ "level is-small mb-1" ] [ inp ]
