module Voxf.EntityState where

import Voxf.Prelude

data EntityState ext = EntityState
    { entityId :: Int
    , position :: Position
    , rotation :: Rotation
    , durability :: Float
    , isStatic :: Bool
    , extraState :: ext
    }
