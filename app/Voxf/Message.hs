module Voxf.Message where

import Voxf.Prelude
import Voxf.ItemDef

data MessageType
    = UseMessageType
    | HitMessageType
    | ExplosionMessageType
    | DestroyMessageType
    | DropItemMessageType
    | ToggleUIMessageType
    | TransferItemRequestMessageType
    | TransferItemSuccessMessageType
    | TransferItemFailureMessageType
  deriving
    (Eq, Show, Read)

data Message
    -- SourceEntity TargetEntity TargetVoxel EquippedItem
    = Use EntityId EntityId
    -- SourceEntity TargetEntity Damage
    | Hit EntityId EntityId Float
    -- SourcePosition Radius Power
    | Explosion (V3 Float) Float Float
    -- Entity
    | Destroy EntityId
    -- Item Qty
    | DropItem ItemDef Qty
    -- SourceEntity TargetEntity SourceSlot TargetSlot ItemId Qty
    | TransferItemRequest TransferItemPayload
    | TransferItemSuccess TransferItemPayload
    | TransferItemFailure TransferItemPayload
    -- TargetEntity IsOpening
    | ToggleUI EntityId Bool

data TransferItemPayload = TransferItemPayload
    { sourceEntity :: EntityId
    , targetEntity :: EntityId
    , sourceSlot :: Index
    , targetSlot :: Index
    , item :: ItemDef
    , itemQty :: Qty
    }

getType :: Message -> MessageType
getType msg = case msg of
    Use _ _               -> UseMessageType
    Hit _ _ _             -> HitMessageType
    Explosion _ _ _       -> ExplosionMessageType
    Destroy _             -> DestroyMessageType
    DropItem _ _          -> DropItemMessageType
    ToggleUI _ _          -> ToggleUIMessageType
    TransferItemRequest _ -> TransferItemRequestMessageType
    TransferItemSuccess _ -> TransferItemSuccessMessageType
    TransferItemFailure _ -> TransferItemFailureMessageType

getTarget :: Message -> Maybe EntityId
getTarget msg = case msg of
    Use _ eid                   -> Just eid
    ToggleUI eid _              -> Just eid
    TransferItemRequest payload -> Just payload.targetEntity
    TransferItemSuccess payload -> Just payload.targetEntity
    TransferItemFailure payload -> Just payload.targetEntity
    _                           -> Nothing
