module Main where

import Voxf.Prelude
import Voxf.Inventory (Inventory)
import Voxf.EntityDef as EntityDef
import Voxf.Entities.Chest as Chest
import qualified Data.Set as Set
import Data.Set (Set)
import Data.IORef
import GHC.Exts (Any)
import System.Mem (performMinorGC)
import Control.Monad
import Graphics.GL
import Graphics.UI.GLFW (Window, Key(..), KeyState(..), MouseButton(..), MouseButtonState(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.Char as Char
import Data.Bool (bool)

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
    , cursorPosition :: V2 Int
    , cursorDelta :: V2 Float
    }
  deriving
    (Show)

emptyPlayerInput = PlayerInput
    { axisForward = 0
    , axisRight = 0
    , isJumping = False
    , isSneaking = False
    , isSprinting = False
    , isPrimaryAction = False
    , isSecondaryAction = False
    , isScrollingUp = False
    , isScrollingDown = False
    , cursorPosition = V2 0 0
    , cursorDelta = V2 0 0
    }

data PlayerAction
    = PlaceBlock
    | DestroyBlock
    | Hit

data RawInput = RawInput
    { leftMouseDown :: Bool
    , rightMouseDown :: Bool
    , mouseX :: Int
    , mouseY :: Int
    , mouseDeltaX :: Float
    , mouseDeltaY :: Float
    , keysDown :: Set Key
    , updatedAt :: Double
    }
  deriving
    (Show)

emptyRawInput :: RawInput
emptyRawInput =
    RawInput
        { leftMouseDown = False
        , rightMouseDown = False
        , mouseX = 0
        , mouseY = 0
        , mouseDeltaX = 0
        , mouseDeltaY = 0
        , keysDown = Set.empty
        , updatedAt = 0
        }

interpretRawInput :: RawInput -> PlayerInput -> PlayerInput
interpretRawInput rawInput prevPlayerInput = do
    PlayerInput
        { isPrimaryAction = rawInput.leftMouseDown
        , isSecondaryAction = rawInput.rightMouseDown
        , axisForward =
            bool 0.0 axisVelocityF (Set.member Key'W rawInput.keysDown) +
            bool 0.0 axisVelocityB (Set.member Key'S rawInput.keysDown)
        , axisRight =
            bool 0.0 axisVelocityR (Set.member Key'D rawInput.keysDown) +
            bool 0.0 axisVelocityL (Set.member Key'A rawInput.keysDown)
        , isJumping = False
        , isSneaking = False
        , isSprinting = False
        , isScrollingUp = False
        , isScrollingDown = False
        , cursorPosition = V2 rawInput.mouseX rawInput.mouseY
        , cursorDelta = V2 rawInput.mouseDeltaX rawInput.mouseDeltaY
        }
    where
        axisVelocityF =  1.0
        axisVelocityB = -1.0
        axisVelocityR =  1.0
        axisVelocityL = -1.0

clearRawInput :: RawInput -> IO ()
clearRawInput = undefined

renderChunk :: GameState -> Chunk -> IO ()
renderChunk state chunk = undefined

data GameState = GameState
    { world :: () -- World
    , visibleChunks :: () -- Vec Chunk
    , player :: () -- Player
    , playerAction :: Maybe PlayerAction
    , pendingMessages :: [Message]
    }

initialState :: GameState
initialState =
    GameState
        { world = ()
        , visibleChunks = ()
        , player = ()
        , playerAction = Nothing
        , pendingMessages = []
        }

getTime :: IO Double
getTime = maybe 0 id <$> GLFW.getTime

data Message = Message

render :: GameState -> IO ()
render = undefined

applyPlayerInput :: PlayerInput -> GameState -> (GameState, [Message])
applyPlayerInput = undefined

applyPlayerCooldowns :: [Message] -> Player -> Player
applyPlayerCooldowns = undefined

updateBlocks :: Float -> GameState -> (GameState, [Message])
updateBlocks = undefined

gameLoop :: (Float -> GameState -> IO GameState) -> IO ()
gameLoop tick = go 0.0 initialState where
    go prevTimestamp state = do
        newTimestamp <- getTime
        let delta = realToFrac $ newTimestamp - prevTimestamp
        newState <- tick delta state
        go newTimestamp newState

registerEntityDefs :: [EntityDef Any Any] -> IO ()
registerEntityDefs = undefined

stdEntityDefs :: [EntityDef Any Any]
stdEntityDefs =
    [ EntityDef.upcast Chest.entityDef ]

setupInputCallbacks :: Window -> IORef RawInput -> IO ()
setupInputCallbacks window rawInputRef = do
    GLFW.setKeyCallback window $ Just $ \_window key _scancode action _mods -> do
        now <- getTime
        modifyIORef rawInputRef $ \rawInput ->
            rawInput
                { keysDown = case action of
                    KeyState'Released -> Set.delete key rawInput.keysDown
                    _                 -> Set.insert key rawInput.keysDown
                , mouseDeltaX = 0
                , mouseDeltaY = 0
                , updatedAt = now
                }
        print =<< readIORef rawInputRef
    GLFW.setMouseButtonCallback window $ Just $ \_window button state _mods -> do
        now <- getTime
        modifyIORef rawInputRef $ \rawInput ->
            case button of
                MouseButton'1 -> rawInput
                    { leftMouseDown = (state == MouseButtonState'Pressed)
                    , mouseDeltaX = 0
                    , mouseDeltaY = 0
                    , updatedAt = now
                    }
                MouseButton'2 -> rawInput
                    { rightMouseDown = (state == MouseButtonState'Pressed)
                    , mouseDeltaX = 0
                    , mouseDeltaY = 0
                    , updatedAt = now
                    }
                _ -> rawInput
        print =<< readIORef rawInputRef
    prevXRef <- newIORef @Double 0.0
    prevYRef <- newIORef @Double 0.0
    GLFW.setCursorPosCallback window $ Just $ \_ x y -> do
        dx <- fmap (\prevX -> x - prevX) $ readIORef prevXRef
        dy <- fmap (\prevY -> y - prevY) $ readIORef prevYRef
        now <- getTime
        modifyIORef rawInputRef $ \rawInput ->
            rawInput
                { mouseX = round x
                , mouseY = round y
                , mouseDeltaX = realToFrac dx
                , mouseDeltaY = realToFrac dy
                , updatedAt = now
                }
        writeIORef prevXRef x
        writeIORef prevYRef y
        print =<< readIORef rawInputRef

main :: IO ()
main = do
    code <- GLFW.init
    putStr "GLFW.init with code: "
    print code
    Just window <- GLFW.createWindow 1024 768 "Voxf" Nothing Nothing
    isRawMouseMotionSupported <- GLFW.rawMouseMotionSupported
    putStr "Raw mouse motion support: "
    print isRawMouseMotionSupported
    -- registerEntityDefs stdEntityDefs
    rawInputRef <- newIORef emptyRawInput
    prevPlayerInputRef <- newIORef emptyPlayerInput
    setupInputCallbacks window rawInputRef
    gameLoop $ \delta gameState -> do
        playerInput <- interpretRawInput
            <$> readIORef rawInputRef
            <*> readIORef prevPlayerInputRef
        writeIORef prevPlayerInputRef playerInput
        -- let (gameState', playerMessages) = applyPlayerInput playerInput gameState
        -- let player' = applyPlayerCooldowns playerMessages (player gameState')
        -- let gameState'' = gameState' {
        --     player = player',
        --     pendingMessages = (pendingMessages gameState') ++ playerMessages
        -- }
        -- let (gameState''', newPendingMessages) = updateBlocks delta gameState''
        -- render
        glClear GL_COLOR_BUFFER_BIT
        GLFW.swapBuffers window
        -- clearRawInput rawInput
        GLFW.pollEvents
        performMinorGC
        return gameState
        -- return $ gameState''' { pendingMessages = newPendingMessages}

-- main :: IO ()
-- main = runIO# $ do
--     code <- glfwInit
--     print "glfwInit: " code
--     glfwDefaultWindowHints
--     window <- glfwCreateWindow 1024# 768# "Unlifted-HSGL" nullAddr# nullAddr#
--     print "glfwCreateWindow: address = " (addr2Int# window)
--     glfwMakeContextCurrent window
--     glfwSetKeyCallback window $ \_ keyCode _ _ _ -> do
--         when (keyCode == charToKeyCode 'w') $ \() ->
--             print "W"
--         when (keyCode == GLFW_KEY_ESCAPE) $ \() ->
--             shutdown window
--     forever $ \() -> do
--         glClearColor 1.0# 1.0# 1.0# 1.0#
--         glClear GL_COLOR_BUFFER_BIT
--         glfwSwapBuffers window
--         glfwPollEvents
