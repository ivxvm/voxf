module Voxf.EntityDef where

import Voxf.Message
import Voxf.MessageBus
import Voxf.EntityState
import GHC.Exts (Any)

data EntityDef = EntityDef
    { initialState :: EntityState Any
    , texture :: ()
    , render :: EntityState Any -> IO ()
    , update :: EntityState Any -> MessageBus -> (EntityState Any, MessageBus)
    , destroy :: EntityState Any -> [Message]
    }
