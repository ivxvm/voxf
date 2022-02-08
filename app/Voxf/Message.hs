module Voxf.Message where

data Message
    = Use {- SourceEntity TargetEntity TargetVoxel EquippedItem -}
    | Hit {- SourceEntity TargetEntity -}
