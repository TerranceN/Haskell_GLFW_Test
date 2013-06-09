{-# LANGUAGE TemplateHaskell #-}
module GSBomberMan
( GSBomberMan
, new
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
import Player

data GSBomberMan = GSBomberMan { _shouldDraw :: Bool
                               , _num :: Int
                               , _gameMap :: Map 
                               , _exitState :: ExitType
                               , _player :: Player
                               } deriving (Eq, Show)
makeLenses ''GSBomberMan

new = GSBomberMan { _num = 1
                  , _gameMap = newMap
                  , _shouldDraw = True
                  , _exitState = NoExit
                  , _player = newPlayer (2, 2)
                  }

instance GameState GSBomberMan where
    initialize = return ()

    handleInput input = do
        self <- get
        if isKeyDown input (GLFW.CharKey 'Y')
            then shouldDraw .= False
            else shouldDraw .= True
        if isKeyDown input (GLFW.SpecialKey GLFW.ESC)
            then exitState .= Exit
            else return ()
        if not (self^.player^.isMoving)
            then do
                let isTileDown = tileIsLower (self^.player^.tilePosition^._1)
                if isKeyDown input (GLFW.CharKey 'D')
                    then do
                        let yOffset = if isTileDown
                            then 1
                            else 0
                        setPlayerNextPosition ((\(x, y) -> (x + 1, y + yOffset)) (self^.player^.tilePosition))
                    else return ()
                if isKeyDown input (GLFW.CharKey 'E')
                    then do
                        let yOffset = if isTileDown
                            then 0
                            else -1
                        setPlayerNextPosition ((\(x, y) -> (x + 1, y + yOffset)) (self^.player^.tilePosition))
                    else return ()
                if isKeyDown input (GLFW.CharKey 'Q')
                    then do
                        let yOffset = if isTileDown
                            then 0
                            else -1
                        setPlayerNextPosition ((\(x, y) -> (x - 1, y + yOffset)) (self^.player^.tilePosition))
                    else return ()
                if isKeyDown input (GLFW.CharKey 'A')
                    then do
                        let yOffset = if isTileDown
                            then 1
                            else 0
                        setPlayerNextPosition ((\(x, y) -> (x - 1, y + yOffset)) (self^.player^.tilePosition))
                    else return ()
                if isKeyDown input (GLFW.CharKey 'W')
                    then do
                        setPlayerNextPosition ((\(x, y) -> (x, y - 1)) (self^.player^.tilePosition))
                    else return ()
                if isKeyDown input (GLFW.CharKey 'S')
                    then do
                        setPlayerNextPosition ((\(x, y) -> (x, y + 1)) (self^.player^.tilePosition))
                    else return ()
            else do
                let char = case (self^.player^.movementDirection) of
                                (0, 1) -> 'W'
                                (0, -1) -> 'S'
                                _ -> '\0'
                if isKeyDown input (GLFW.CharKey char)
                    then do
                        player .= flipDirections (self^.player)
                    else return ()

    update = do
        self <- get
        player .= execState updatePlayer (self^.player)

    draw = do
        self <- get
        lift $ do
            if self^.shouldDraw
                then do
                    GL.loadIdentity
                    GL.translate $ GL.Vector3 tileHexRadius tileHexRadius 0
                    renderMap (self^.gameMap)
                    runStateT drawPlayer (self^.player)
                    return ()
                else return ()

    shouldExit = liftM (^.exitState) get

    getNextState = return Nothing

setPlayerNextPosition pos = do
    self <- get
    case (tileAt pos (self^.gameMap)) of
        Just tile -> case tile^.tileType of
                        NormalTile -> player.nextTilePosition .= pos
                        _ -> return ()
        Nothing -> return ()
