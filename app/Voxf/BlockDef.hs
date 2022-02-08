module Voxf.BlockDef where

import Voxf.BlockState as BlockState

data BlockDef = BlockDef
    { initialState :: BlockState
    , update :: Message -> BlockState -> (BlockState, [Message])
    , destroy :: BlockState -> [Message]
    }
