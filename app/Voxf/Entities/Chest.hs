module Voxf.Entities.Chest (entityDef) where

import Voxf.Prelude
import Voxf.Types
import Voxf.Message as Message
import Voxf.MessageMap as MessageMap
import Voxf.Inventory as Inventory
import Data.Monoid (Sum(..))
import Data.Foldable
import Linear as Lin
import Control.Monad.Writer.Strict

pattern OPENING_ANIMATION_DURATION = 1.0
pattern CLOSING_ANIMATION_DURATION = 1.0

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
        , contents = Inventory.init 96
        }
    }

calcDamage :: MessageMap -> EntityId -> Position -> Float
calcDamage inMessages entityId entityPos = hitDamage + explosionDamage
    where
        msgToSum (Hit _ _ damage) = Sum damage
        msgToSum (Explosion sourcePos radius damage)
            | Lin.distance entityPos sourcePos < radius = Sum damage
            | otherwise                                 = Sum 0
        msgToSum _ = Sum 0
        hitDamage = getSum $ foldMap' msgToSum $ MessageMap.getByTarget entityId inMessages
        explosionDamage = getSum $ foldMap' msgToSum $ MessageMap.getByType ExplosionMessageType inMessages

updateAnimation :: DeltaTime -> MessageMap -> EntityId -> ChestAnimationState -> ChestAnimationState
updateAnimation delta inMessages entityId animState = newAnimState
    where
        isPlayerInteracting = not $ null $
            MessageMap.getByTargetAndType entityId UseMessageType inMessages
        isPlayerTogglingUI = not $ null $
            MessageMap.getByTargetAndType entityId ToggleUIMessageType inMessages
        newAnimState = case (animState, isPlayerInteracting || isPlayerTogglingUI) of
            (Closed, True) -> Opening 0
            (Open,   True) -> Closing 0
            (Opening t, _)
                | t < OPENING_ANIMATION_DURATION -> Opening (t + delta)
                | otherwise                      -> Open
            (Closing t, _)
                | t < CLOSING_ANIMATION_DURATION -> Closing (t + delta)
                | otherwise                      -> Closed
            _ -> animState

-- updateInventory :: MessageMap -> EntityId -> Inventory -> (Inventory, [Message])
-- updateInventory inMessages entityId inventory = (newInventory, outMessages)
--     where
--         transferRequests = MessageMap.getByTargetAndType entityId TransferItemRequestMessageType


update :: DeltaTime -> MessageMap -> EntityState ChestState -> (EntityState ChestState, [Message])
update delta inMessages state = (newState, outMessages)
    where
        newDurability = state.durability - calcDamage inMessages state.entityId state.position
        newInventory = state.extraState.contents
        newAnimState = updateAnimation delta inMessages state.entityId state.extraState.animState
        newState = state
            { durability = newDurability
            , extraState = state.extraState { animState = newAnimState } }
        outMessages = execWriter $ do
            when (newDurability <= 0) $ do
                tell [Destroy state.entityId]
                tell $ map (\ii -> DropItem ii.item ii.stackSize) $ Inventory.listItems newInventory

entityDef :: EntityDef ChestState
entityDef = EntityDef
    { initialState = Voxf.Entities.Chest.initialState
    , texture = ()
    , render = \_ _ -> return ()
    , update = Voxf.Entities.Chest.update
    }
