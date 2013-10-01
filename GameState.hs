{-# LANGUAGE TemplateHaskell #-}
module GameState where

import Data.IORef
import Control.Lens
import Control.Monad.Trans.State
import qualified Graphics.Rendering.OpenGL as GL

import Input

type NextState = IORef Input -> IO ExitType
type StateUpdate a r = State a r
type StateUpdateIO a r = StateT a IO r

data ExitType = NoExit
              | Exit
              | ExitAll
              deriving (Eq, Show)

class GameState a where
    initialize :: StateUpdateIO a ()
    update :: StateUpdate a ()
    handleInput :: Input -> StateUpdate a ()
    draw :: StateUpdateIO a ()
    shouldExit :: StateUpdateIO a ExitType
    getNextState :: StateUpdate a (Maybe (IORef Input -> IO ExitType))
