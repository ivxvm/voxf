module Voxf.Inventory where

import Voxf.Prelude
import Voxf.InventoryItem

data Inventory = Inventory
    { slotsCount :: Int
    , items :: [InventoryItem]
    }
