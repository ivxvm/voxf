module Voxf.EntityDef where

import Voxf.Prelude
import Voxf.Message
import Voxf.MessageMap
import Voxf.EntityState
import Voxf.RenderContext
import Unsafe.Coerce

data EntityDef est rst = EntityDef
    { name :: Text
    , texture :: Text
    , initEntityState :: EntityState est
    , initRenderState :: Text -> IO rst
    , renderBatch :: Maybe (DeltaTime -> RenderContext -> EntityState est -> IO ())
    , renderSingle :: DeltaTime -> RenderContext -> EntityState est -> IO ()
    , update :: DeltaTime -> MessageMap -> EntityState est -> (EntityState est, [Message])
    }

upcast :: EntityDef est rst -> EntityDef Any Any
upcast = unsafeCoerce
{-# INLINE upcast #-}

downcast :: EntityDef Any Any -> EntityDef est rst
downcast = unsafeCoerce
{-# INLINE downcast #-}
