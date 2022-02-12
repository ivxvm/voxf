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
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Graphics.GL

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

data ChestRenderState = ChestRenderState
    { texture :: GLuint
    }

initEntityState :: EntityState ChestState
initEntityState = EntityState
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

initRenderState :: String -> IO ChestRenderState
initRenderState texturePath = do
    t <- alloca $ \ptr ->
        glGenTextures 1 ptr >> peek ptr
    return $
        ChestRenderState
            { texture = t }

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

updateInventory :: MessageMap -> EntityId -> Inventory -> (Inventory, [Message])
updateInventory inMessages entityId inventory = applyTransfers inventory transferPayloads []
    where
        transferPayloads = map (\(TransferItemRequest p) -> p) $
            MessageMap.getByTargetAndType entityId TransferItemRequestMessageType inMessages
        applyTransfers inventory []               outMessages = (inventory, outMessages)
        applyTransfers inventory (payload : rest) outMessages =
            case Inventory.insert payload.targetSlot payload.item payload.itemQty inventory of
                (0, inventory) -> applyTransfers inventory rest $
                    TransferItemFailure payload : outMessages
                (n, inventory) -> applyTransfers inventory rest $
                    TransferItemSuccess (payload { itemQty = n }) : outMessages

update :: DeltaTime -> MessageMap -> EntityState ChestState -> (EntityState ChestState, [Message])
update delta inMessages state = (newState, outMessages)
    where
        newDurability = state.durability - calcDamage inMessages state.entityId state.position
        (newInventory, inventoryOutMessages) =
            updateInventory inMessages state.entityId state.extraState.contents
        newAnimState = updateAnimation delta inMessages state.entityId state.extraState.animState
        newState = state
            { durability = newDurability
            , extraState = state.extraState
                { animState = newAnimState
                , contents = newInventory
                }
            }
        outMessages = execWriter $ do
            when (newDurability <= 0) $ do
                tell [Destroy state.entityId]
                tell $ map (\ii -> DropItem ii.item ii.stackSize) $ Inventory.listItems newInventory
            tell inventoryOutMessages

entityDef :: EntityDef ChestState ChestRenderState
entityDef = EntityDef
    { initEntityState = Voxf.Entities.Chest.initEntityState
    , initRenderState = Voxf.Entities.Chest.initRenderState
    , texture = "chest.png"
    , renderBatch = Nothing
    , renderSingle = \_ _ _ -> return ()
    , update = Voxf.Entities.Chest.update
    }
