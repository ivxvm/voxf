module Voxf.Prelude (
    module Voxf.Prelude,
    Any, Text,
    V2(..), V3(..), V4(..), Quaternion(..),
    (&)
) where

import GHC.Exts (Any)
import Data.Text (Text, pack)
import Data.Function ((&))
import Linear

type EntityId = Int
type ItemId = Int
type Index = Int
type Qty = Int
type DeltaTime = Float
type Position = V3 Float
type Rotation = Quaternion Float

fromString :: String -> Text
fromString = pack
{-# INLINE fromString #-}
