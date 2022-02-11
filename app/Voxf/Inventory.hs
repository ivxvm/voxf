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

insert :: Index -> ItemDef -> Qty -> Inventory -> (Qty, Inventory)
insert slot item itemQty inventory
    | slot < 0 || slot >= inventory.slotsCount = (0, inventory)
    | otherwise                                = (insertedQty, newInventory) where
        newValue = InventoryItem item (min itemQty item.maxStackSize)
        (maybeOldValue, newItems) =
            IntMap.insertLookupWithKey combine slot newValue inventory.items
        insertedQty = case maybeOldValue of
            Nothing -> newValue.stackSize
            Just oldValue
                | newValue.item == oldValue.item -> combineStackSize newValue oldValue
                | otherwise                      -> 0
        newInventory
            | insertedQty > 0 = inventory { items = newItems }
            | otherwise       = inventory
        combineStackSize new old =
            min new.item.maxStackSize (old.stackSize + new.stackSize)
        combine _ new old
            | new.item == old.item = InventoryItem new.item (combineStackSize new old)
            | otherwise            = old


insertExact :: Index -> ItemDef -> Qty -> Inventory -> Maybe Inventory
insertExact slot item itemQty inventory
    | slot > inventory.slotsCount = Nothing
    | otherwise = undefined

listItems :: Inventory -> [InventoryItem]
listItems inventory = IntMap.elems inventory.items
