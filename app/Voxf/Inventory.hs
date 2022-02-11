module Voxf.Inventory where

import Voxf.Prelude
import Voxf.ItemDef
import Voxf.InventoryItem
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict as IntMap

data Inventory = Inventory
    { slotsCount :: Int
    , items :: IntMap InventoryItem
    }

init :: Int -> Inventory
init slotsCount = Inventory
    { slotsCount = slotsCount
    , items = IntMap.empty }

insertExact :: Index -> ItemDef -> Qty -> Inventory -> Maybe Inventory
insertExact slot item itemQty inventory
    | slot > inventory.slotsCount = Nothing
    | otherwise = Just inventory

listItems :: Inventory -> [InventoryItem]
listItems inventory = IntMap.elems inventory.items
