module Voxf.InventoryItem (InventoryItem(..)) where

data InventoryItem = InventoryItem
    { itemId :: Int
    , itemCount :: Int
    }
