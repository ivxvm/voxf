module Main where

import Voxf.Prelude
import Voxf.EntityDef (EntityDef)
import Voxf.Inventory (Inventory)
import Data.Map (Map)
import GHC.Exts (Any)
import System.Mem (performMinorGC)
import Control.Monad
import Graphics.GL

data Vec a = Vec a

data Octree = Octree

data World = World

data Chunk = Chunk
    { chunkId :: Int
    , chunkVoxels :: Octree
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

registerEntityDefs :: [EntityDef Any Any] -> IO ()
registerEntityDefs = undefined

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
        -- render
        glClear GL_COLOR_BUFFER_BIT
        performMinorGC
        return $ gameState''' { pendingMessages = newPendingMessages}
