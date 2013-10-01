module Input
( Input
, emptyInput
, registerInputCallbacks
, isKeyDown
) where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef

data Input = Input { keysDown :: [GLFW.Key]
                   , keysDownOnce :: [GLFW.Key]
                   } deriving (Eq, Show)

emptyInput :: Input
emptyInput = Input { keysDown = []
                   , keysDownOnce = []
                   }

registerInputCallbacks :: IO (IORef Input)
registerInputCallbacks = do
    ioRef <- newIORef emptyInput
    GLFW.keyCallback $= (handleKeyEvent ioRef)
    return ioRef

addKey :: GLFW.Key -> Input -> Input
addKey key input = input { keysDown = key:(keysDown input) }

removeKey :: GLFW.Key -> Input -> Input
removeKey key input = input { keysDown = filter (\x -> x /= key) (keysDown input) }

isKeyDown input key = elem key (keysDown input)

handleKeyEvent :: IORef Input -> GLFW.Key -> GLFW.KeyButtonState -> IO ()
handleKeyEvent inputRef key state = do
    case state of
        GLFW.Press -> modifyIORef inputRef (addKey key)
        GLFW.Release -> modifyIORef inputRef (removeKey key)
