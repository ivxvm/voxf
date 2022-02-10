module Voxf.InventoryItem (InventoryItem(..)) where

import Voxf.Prelude

data InventoryItem = InventoryItem
    { itemId :: Int
    , itemCount :: Int
    }
