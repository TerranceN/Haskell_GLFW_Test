{-# LANGUAGE TemplateHaskell #-}
module GSBomberMan
( GSBomberMan
, new
, module LensHelpers
, module Input
) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import GameState
import Map
import Tile
import Input
import qualified Player
import LensHelpers

data GSBomberMan = GSBomberMan { _shouldDraw :: Bool
                               , _num :: Int
                               , _gameMap :: Map 
                               , _exitState :: ExitType
                               , _player :: Player.Player
                               } deriving (Eq, Show)
makeLenses ''GSBomberMan

new = GSBomberMan { _num = 1
                  , _gameMap = newMap
                  , _shouldDraw = True
                  , _exitState = NoExit
                  , _player = Player.new (2, 2)
                  }

instance GameState GSBomberMan where
    initialize = return ()

    handleInput input = do
        self <- get
        if isKeyDown input (GLFW.CharKey 'Y')
            then shouldDraw .= False
            else shouldDraw .= True
        if isKeyDown input (GLFW.SpecialKey GLFW.ESC)
            then exitState .= ExitAll
            else return ()
        player.-Player.handleInput input (self^.gameMap)

    update = do
        self <- get
        player.-Player.update

    draw = do
        self <- get
        lift $ do
            if self^.shouldDraw
                then do
                    GL.loadIdentity
                    GL.translate $ GL.Vector3 tileHexRadius tileHexRadius 0
                    renderMap (self^.gameMap)
                    runStateT Player.draw (self^.player)
                    return ()
                else return ()

    shouldExit = liftM (^.exitState) get

    getNextState = return Nothing

