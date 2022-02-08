module Voxf.Entities.Chest where -- (entityDef) where

import Voxf.Prelude
import Voxf.EntityState
import Voxf.Message
import Voxf.MessageBus as MessageBus
import Voxf.Inventory

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
    { durability = 100
    , rotation = ()
    , extraState = ChestState
        { animState = Closed
        , contents = Inventory
            { slotsCount = 96
            , items = []
            }
        }
    }

updateDurability :: DeltaTime -> Int -> MessageBus -> Int
updateDurability delta durability msgs = undefined

update :: DeltaTime -> EntityState ChestState -> MessageBus -> (EntityState ChestState, MessageBus)
update delta state msgs = (newState, MessageBus.empty)
    where
        newState = state
            { durability = updateDurability delta state.durability msgs }

-- entityDef :: EntityDef ChestState
-- entityDef = EntityDef
--     { initialState = EntityState Any
--     , texture = ()
--     , render = EntityState -> IO ()
--     , update  Message -> EntityState -> (EntityState, [Message])
--     , destroy :: EntityState -> [Message]
--     }
