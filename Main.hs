module Main where

import Window
import Input
import RunGameState
import qualified GSBomberMan

main = do
    initWindow
    inputIORef <- registerInputCallbacks
    runGameState GSBomberMan.new inputIORef 
    closeWindow
