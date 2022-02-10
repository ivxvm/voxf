module Voxf.MessageMap where

import Voxf.Message

data MessageMap = MessageMap

empty :: MessageMap
empty = MessageMap

getByTargetEntityId :: Int -> MessageMap -> [Message]
getByTargetEntityId = undefined

getByMessageType :: MessageType -> MessageMap -> [Message]
getByMessageType = undefined
