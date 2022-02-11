module Voxf.EntityDef where

import Voxf.Prelude
import Voxf.Message
import Voxf.MessageMap
import Voxf.EntityState

data EntityDef st = EntityDef
    { initialState :: EntityState st
    , texture :: ()
    , render :: DeltaTime -> EntityState st -> IO ()
    , update :: DeltaTime -> MessageMap -> EntityState st -> (EntityState st, [Message])
    }
