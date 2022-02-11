module Voxf.Prelude (
    module Voxf.Prelude,
    V2(..), V3(..), V4(..), Quaternion(..),
    (&)
) where

import Linear
import Data.Function ((&))

type EntityId = Int
type ItemId = Int
type Index = Int
type Qty = Int
type DeltaTime = Float
type Position = V3 Float
type Rotation = Quaternion Float
