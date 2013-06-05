{-# LANGUAGE TemplateHaskell #-}
module GameState where

import Data.IORef
import Control.Lens
import Control.Monad.Trans.State
import qualified Graphics.Rendering.OpenGL as GL

import Input

data ExitType = NoExit
              | Exit
              | ExitAll

class GameState a where
    initialize :: StateT a IO ()
    update :: State a ()
    handleInput :: Input -> State a ()
    draw :: StateT a IO ()
    shouldExit :: StateT a IO ExitType
    getNextState :: State a (Maybe (IORef Input -> IO ExitType))
