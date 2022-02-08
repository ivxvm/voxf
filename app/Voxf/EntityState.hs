module Voxf.EntityState where

data EntityState ext = EntityState
    { entityId :: Int
    , durability :: Int
    , rotation :: ()
    , extraState :: ext
    }
