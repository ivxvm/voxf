module Main where

import Control.Monad
import Data.Map (Map)

data Vec a = Vec a

data Octree = Octree

data World = World

data Chunk = Chunk
    { chunkId :: Int
    , chunkVoxels :: Octree
    }

data InventoryItem = InventoryItem
    { invItemId :: Int
    , invItemCount :: Int
    }

data Inventory = Inventory
    { invSlotsCount :: Int
    , invItems :: Vec InventoryItem
    }

data Player = Player
    { backpackInventory :: Inventory
    , equipmentInventory :: Inventory
    , hotbarInventory :: Inventory
    , selectedHotbarslot
    , health :: Float
    , mana :: Float
    }

data PlayerInput = PlayerInput
    { axisForward :: Float
    , axisRight :: Float
    , isJumping :: Bool
    , isSneaking :: Bool
    , isSprinting :: Bool
    , isPrimaryAction :: Bool
    , isSecondaryAction :: Bool
    , isScrollingUp :: Bool
    , isScrollingDown :: Bool
    }

data PlayerAction
    = PlaceBlock
    | DestroyBlock
    | Hit

data RawInput = RawInput
    { leftMouseDown :: Bool
    , rightMouseDown :: Bool
    , keyDown :: Map Char Bool
    }

initRawInput :: IO RawInput
initRawInput = undefined

collectRawInput :: RawInput -> IO PlayerInput
collectRawInput = undefined

clearRawInput :: RawInput -> IO ()
clearRawInput = undefined

renderChunk :: GameState -> Chunk -> IO ()
renderChunk state chunk = undefined

setupInputCallbacks :: RawInput -> IO ()
setupInputCallbacks = undefined

pollEvents :: IO ()
pollEvents = undefined

data GameState = GameState
    { world :: World
    , visibleChunks :: Vec Chunk
    , player :: Player
    , playerAction :: Maybe PlayerAction
    , pendingMessages :: [Message]
    }

data Block = Block

-- data DropItem = DropItem
--     { itemId :: Int
--     , minQty :: Int
--     , maxQty :: Int
--     , probability :: Int
--     }

initialState :: GameState
initialState = undefined

data Message = Message

render :: GameState -> IO ()
render = undefined

applyPlayerInput :: PlayerInput -> GameState -> (GameState, [Message])
applyPlayerInput = undefined

applyPlayerCooldowns :: [Message] -> Player -> Player
applyPlayerCooldowns = undefined

getTime :: IO Float
getTime = undefined

updateBlocks :: Float -> GameState -> (GameState, [Message])
updateBlocks = undefined

gameLoop :: (Float -> GameState -> IO GameState) -> IO ()
gameLoop tick = go 0.0 initialState where
    go prevTimestamp state = do
        newTimestamp <- getTime
        let delta = newTimestamp - prevTimestamp
        newState <- tick delta state
        go newTimestamp newState

main :: IO ()
main = do
    rawInput <- initRawInput
    setupInputCallbacks rawInput
    gameLoop $ \delta gameState -> do
        clearRawInput rawInput
        pollEvents
        playerInput <- collectRawInput rawInput
        let (gameState', playerMessages) = applyPlayerInput playerInput gameState
        let player' = applyPlayerCooldowns playerMessages (player gameState')
        let gameState'' = gameState' {
            player = player',
            pendingMessages = (pendingMessages gameState') ++ playerMessages
        }
        let (gameState''', newPendingMessages) = updateBlocks delta gameState''
        return $ gameState''' { pendingMessages = newPendingMessages}
