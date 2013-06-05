module RunGameState where

import System.TimeIt
import Data.IORef
import Control.Monad.Trans.State
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import Input
import GameState

frameRate = 60.0
delayTime = 1.0 / frameRate

runGameState :: GameState a => a -> IORef Input -> IO ExitType
runGameState state inputIORef = do
    initState <- execStateT initialize state
    updateDraw inputIORef initState
  where
    updateDraw :: GameState a => IORef Input -> a -> IO ExitType
    updateDraw inputIORef state = do
        (time, newState) <- timeItT $ do
            input <- readIORef inputIORef
            let inputFinishedState = execState (handleInput input) state
            let newState = execState update inputFinishedState
            GL.clear [GL.ColorBuffer]
            runStateT draw newState
            GLFW.swapBuffers
            return newState

        if time < delayTime
            then GLFW.sleep (delayTime - time)
            else return ()

        let nextState = evalState getNextState newState
        case nextState of
            Just comp -> do
                result <- comp inputIORef
                case result of
                    ExitAll -> return ExitAll
                    _ -> loop inputIORef newState 
            Nothing -> loop inputIORef newState
    loop :: GameState a => IORef Input -> a -> IO ExitType
    loop inputIORef newState = do
        windowOpen <- GLFW.getParam GLFW.Opened
        if windowOpen
            then do
                -- check for exit condition and either recurse or exit
                finished <- evalStateT shouldExit newState
                case finished of
                    NoExit -> updateDraw inputIORef newState
                    _ -> return finished
            else return ExitAll
