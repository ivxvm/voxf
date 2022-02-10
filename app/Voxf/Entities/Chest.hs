module Voxf.Entities.Chest where -- (entityDef) where

import Voxf.Prelude
import Voxf.EntityState
import Voxf.Message
import Voxf.MessageMap as MessageMap
import Voxf.Inventory
import Data.Monoid (Sum(..))
import Data.Foldable

data ChestAnimationState
    = Closed
    | Opening Float
    | Open
    | Closing Float

data ChestState = ChestState
    { animState :: ChestAnimationState
    , contents :: Inventory
    }

initialState :: EntityState ChestState
initialState = EntityState
    { entityId = -1
    , position = V3 0 0 0
    , rotation = Quaternion 0 (V3 0 1 0)
    , durability = 100
    , isStatic = True
    , extraState = ChestState
        { animState = Closed
        , contents = Inventory
            { slotsCount = 96
            , items = []
            }
        }
    }

calcDamage :: EntityId -> MessageMap -> Float
calcDamage entityId msgs = hitDamage + explosionDamage
    where
        msgToSum (Hit _ _ damage)     = Sum damage
        msgToSum (Explosion _ damage) = Sum damage
        msgToSum _                    = Sum 0
        hitDamage = getSum $ foldMap' msgToSum $ MessageMap.getByTargetEntityId entityId msgs
        explosionDamage = getSum $ foldMap' msgToSum $ MessageMap.getByMessageType ExplosionMessageType msgs


update :: DeltaTime -> EntityState ChestState -> MessageMap -> (EntityState ChestState, [Message])
update delta state msgs = (newState, [])
    where
        newState = state
            { durability = state.durability - calcDamage state.entityId msgs }

-- entityDef :: EntityDef ChestState
-- entityDef = EntityDef
--     { initialState = EntityState Any
--     , texture = ()
--     , render = EntityState -> IO ()
--     , update  Message -> EntityState -> (EntityState, [Message])
--     , destroy :: EntityState -> [Message]
--     }
