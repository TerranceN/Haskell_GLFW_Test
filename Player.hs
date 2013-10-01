{-# LANGUAGE TemplateHaskell #-}
module Player
( Player
, tilePosition
, nextTilePosition
, isMoving

, movementDirection

, new
, flipDirections
, update
, handleInput
, setPosition
, setNextPosition
, draw
) where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import Map
import LensHelpers
import Input
import Tile

data Player = Player { _tilePosition :: (Int, Int)
                     , _nextTilePosition :: (Int, Int)
                     , _percentageThere :: GL.GLfloat
                     , _isMoving :: Bool
                     } deriving (Eq, Show)
makeLenses ''Player

new pos = Player { _tilePosition = pos
                 , _nextTilePosition = pos
                 , _percentageThere = 0
                 , _isMoving = False
                 }

handleInput input gameMap = do
    self <- get
    if not (self^.isMoving)
        then do
            let isTileDown = tileIsLower (self^.tilePosition^._1)
            if isKeyDown input (GLFW.CharKey 'D')
                then do
                    let yOffset = if isTileDown
                                    then 1
                                    else 0
                    setPlayerNextPosition gameMap (offset (self^.tilePosition) (1, yOffset))
                else return ()
            if isKeyDown input (GLFW.CharKey 'E')
                then do
                    let yOffset = if isTileDown
                                    then 0
                                    else -1
                    setPlayerNextPosition gameMap (offset (self^.tilePosition) (1, yOffset))
                else return ()
            if isKeyDown input (GLFW.CharKey 'Q')
                then do
                    let yOffset = if isTileDown
                                    then 0
                                    else -1
                    setPlayerNextPosition gameMap (offset (self^.tilePosition) (-1, yOffset))
                else return ()
            if isKeyDown input (GLFW.CharKey 'A')
                then do
                    let yOffset = if isTileDown
                                    then 1
                                    else 0
                    setPlayerNextPosition gameMap (offset (self^.tilePosition) (-1, yOffset))
                else return ()
            if isKeyDown input (GLFW.CharKey 'W')
                then do
                    setPlayerNextPosition gameMap (offset (self^.tilePosition) (0, -1))
                else return ()
            if isKeyDown input (GLFW.CharKey 'S')
                then do
                    setPlayerNextPosition gameMap (offset (self^.tilePosition) (0, 1))
                else return ()
        else do
            let char = case (self^.movementDirection) of
                            (0, 1) -> 'W'
                            (0, -1) -> 'S'
                            _ -> '\0'
            if isKeyDown input (GLFW.CharKey char)
                then do
                    flipDirections
                else return ()

offset (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

setPlayerNextPosition gameMap pos = do
    case (tileAt pos gameMap) of
        Just tile -> case tile^.tileType of
                        NormalTile -> nextTilePosition .= pos
                        _ -> return ()
        Nothing -> return ()

update :: State Player ()
update = do
    self <- get
    if (self^.nextTilePosition) /= (self^.tilePosition)
        then do
            isMoving .= True
            percentageThere += 0.1

            if (self^.percentageThere) > 1
                then do
                    tilePosition .= self^.nextTilePosition
                    percentageThere .= 0
                    isMoving .= False
                else return ()
        else return ()

setPosition :: (Int, Int) -> State Player ()
setPosition pos = do
    tilePosition .= pos
    nextTilePosition .= pos

setNextPosition :: (Int, Int) -> State Player ()
setNextPosition pos = do
    nextTilePosition .= pos

flipDirections :: State Player ()
flipDirections = do
    self <- get
    let tmp = self^.tilePosition
    tilePosition .= self^.nextTilePosition
    nextTilePosition .= tmp
    percentageThere .= 1 - (self^.percentageThere)

movementDirection :: Lens Player Player (Int, Int) a
movementDirection = lens diff (\player v -> player)
  where
    diff player = (nx - x, ny - y)
      where
        x = player^.tilePosition^._1
        y = player^.tilePosition^._2
        nx = player^.nextTilePosition^._1
        ny = player^.nextTilePosition^._2

draw :: StateT Player IO ()
draw = do
    self <- get
    lift $ do
        GL.renderPrimitive GL.Quads $ do
            let size = 20
            let (tileX, tileY) = gridToMap (self^.tilePosition)
            let (nextX, nextY) = gridToMap (self^.nextTilePosition)
            let diffX = nextX - tileX
            let diffY = nextY - tileY
            let x = tileX + diffX * self^.percentageThere
            let y = tileY + diffY * self^.percentageThere
            GL.vertex $ GL.Vertex2 (x - size) (y - size)
            GL.vertex $ GL.Vertex2 (x + size) (y - size)
            GL.vertex $ GL.Vertex2 (x + size) (y + size)
            GL.vertex $ GL.Vertex2 (x - size) (y + size)
