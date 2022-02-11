module Voxf.InventoryItem (InventoryItem(..)) where

import Voxf.Prelude
import Voxf.ItemDef

data InventoryItem = InventoryItem
    { item :: ItemDef
    , stackSize :: Int
    }
