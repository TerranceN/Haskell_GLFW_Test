module Main where

import Window
import Input
import RunGameState
import qualified GSBomberMan

main = do
    initWindow
    inputIORef <- registerInputCallbacks
    test <- runGameState GSBomberMan.new inputIORef 
    putStrLn $ show test
    closeWindow
