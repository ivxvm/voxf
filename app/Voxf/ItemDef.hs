module Voxf.ItemDef where

data ItemDef = ItemDef
    { name :: String
    , description :: String
    , maxStackSize :: Int
    }
  deriving
    (Eq, Ord, Show, Read)
