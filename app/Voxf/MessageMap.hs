module Voxf.MessageMap where

import Voxf.Prelude
import Voxf.Message as Message

type MessageMap = [Message]

empty :: MessageMap
empty = []

getByTarget :: EntityId -> MessageMap -> [Message]
getByTarget entityId messages = filter predicate messages
    where
        predicate msg =
            Message.getTarget msg == Just entityId

getByType :: MessageType -> MessageMap -> [Message]
getByType messageType messages = filter predicate messages
    where
        predicate msg =
            Message.getType msg == messageType

getByTargetAndType :: EntityId -> MessageType -> MessageMap -> [Message]
getByTargetAndType entityId messageType messages = filter predicate messages
    where
        predicate msg =
            Message.getTarget msg == Just entityId &&
            Message.getType msg == messageType
