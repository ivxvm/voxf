module Voxf.ItemDef where

data ItemDef = ItemDef
    { id :: Int
    , title :: String
    , description :: String
    , maxStackSize :: Int
    }
  deriving
    (Show, Read)

instance Eq ItemDef where
    lhs == rhs = lhs.id == rhs.id
