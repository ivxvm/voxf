module Voxf.MessageBus where

import Voxf.Message

data MessageBus = MessageBus

empty :: MessageBus
empty = MessageBus

getByTargetEntityId :: Int -> MessageBus -> [Message]
getByTargetEntityId = undefined
