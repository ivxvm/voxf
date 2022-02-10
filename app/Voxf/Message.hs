module Voxf.Message where

import Voxf.Prelude

data MessageType
    = UseMessageType
    | HitMessageType
    | ExplosionMessageType

data Message
    -- SourceEntity TargetEntity TargetVoxel EquippedItem
    = Use
    -- SourceEntity TargetEntity Damage
    | Hit EntityId EntityId Float
    -- SourcePosition Power
    | Explosion () Float
