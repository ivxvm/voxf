module Voxf.EntityDef where

import Voxf.Message
import Voxf.MessageMap
import Voxf.EntityState

data EntityDef st = EntityDef
    { initialState :: EntityState st
    , texture :: ()
    , render :: EntityState st -> IO ()
    , update :: EntityState st -> MessageMap -> (EntityState st, [Message])
    , destroy :: EntityState st -> [Message]
    }
