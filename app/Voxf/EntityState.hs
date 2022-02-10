module Voxf.EntityState where

import Voxf.Prelude

data EntityState ext = EntityState
    { entityId :: Int
    , position :: V3 Float
    , rotation :: Quaternion Float
    , durability :: Float
    , isStatic :: Bool
    , extraState :: ext
    }
