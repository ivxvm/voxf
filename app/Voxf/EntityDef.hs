module Voxf.EntityDef where

import Voxf.Prelude
import Voxf.Message
import Voxf.MessageMap
import Voxf.EntityState
import Voxf.RenderContext

data EntityDef est rst = EntityDef
    { initEntityState :: EntityState est
    , initRenderState :: String -> IO rst
    , texture :: String
    , renderBatch :: Maybe (DeltaTime -> RenderContext -> EntityState est -> IO ())
    , renderSingle :: DeltaTime -> RenderContext -> EntityState est -> IO ()
    , update :: DeltaTime -> MessageMap -> EntityState est -> (EntityState est, [Message])
    }
