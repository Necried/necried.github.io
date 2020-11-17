{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Memory where

import           CPUComponent
import           Control.Lens
import qualified Data.IntMap.Strict as IM
import           Data.Word
import           Helpers
import           Miso               hiding (at)
import           Miso.String        hiding (map)
import           Text.Read

data MemorySlot =
    MemorySlot {
    _memoryData    :: !Word64
  , _selectedState :: SelectedState
  } deriving (Eq, Show)

newtype InputAllowed = InputAllowed { _unInputState :: Bool } deriving (Eq, Show)

data Memory = Memory { _memorySlot :: IM.IntMap MemorySlot, _inputAllowed :: InputAllowed } deriving (Show, Eq)

makeLenses ''InputAllowed
makeLenses ''MemorySlot
makeLenses ''Memory

initialMemorySlot :: MemorySlot
initialMemorySlot = MemorySlot 0 NotSelected

instance CPUComponent MemorySlot where
    makeSelected m = m & selectedState .~ Selected
    makeNotSelected m = m & selectedState .~ NotSelected



data Action = OpenEditData Int | DeleteSlot Int deriving (Eq, Show)

initialMemory = Memory IM.empty (InputAllowed True)

data Interface action
    = Interface {
        passAction   :: Action -> action
        , modifyData :: Int -> MisoString -> action
    }

updateMemory :: Interface action -> Action -> Miso.Transition action Memory ()
updateMemory _ action = case action of
    DeleteSlot idx ->
        memorySlot %= IM.delete idx
    OpenEditData idx ->
        pure ()

viewMemory :: Interface action -> Memory -> [View action]
viewMemory iface m =
    map inp $ IM.keys (m ^. memorySlot)
  where
    memLabel lab = span_ [class_ $ highlightSelected lab]
                     [ text $ showm lab ]
    lvlItem = div_ [ class_ "level-item" ]
    inp lab = div_ [ class_ "level-left" ]
        [ lvlItem [ p_ [ class_ "subtitle is-6" ] [ memLabel lab ] ]
        , lvlItem [ p_ [ class_ "control" ]
                        [ input_ [ class_ "input is-small"
                                , type_ "text"
                                , value_ $ showm (m ^. memorySlot . at lab . non initialMemorySlot . memoryData)
                                , disabled_ True
                                ]
                        ]
                ]
        , hasEditIcon lab
        , hasTrashIcon lab
        ]
    highlightSelected idx = case m ^. memorySlot . at idx . non initialMemorySlot . selectedState of
        Selected    -> "tag is-warning"
        NotSelected -> "tag is-black"
    hasTrashIcon lab = if m ^. inputAllowed . unInputState
        then mkIcon (Just (passAction iface $ DeleteSlot lab)) (Just IsLarge) TextDanger "fa fa-trash"
        else span_ [] []
    hasEditIcon lab = if m ^. inputAllowed . unInputState
        then mkIcon (Just $ modifyData iface lab $ showm (m ^. memorySlot . at lab . non initialMemorySlot . memoryData))
                     (Just IsLarge) TextInfo "fa fa-edit"
        else span_ [] []
