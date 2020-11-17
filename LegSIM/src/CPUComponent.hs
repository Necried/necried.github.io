{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module CPUComponent where

import           Control.Lens (makeLenses, (&), (.=), (.~), (^.))
import           Data.Word
import           Helpers
import           Miso
import           Miso.String

-- TODO: Possibly add a "Hover" state?
data SelectedState = Selected | NotSelected
    deriving ( Show, Eq )

class CPUComponent a where
  makeSelected :: a -> a
  makeNotSelected :: a -> a
