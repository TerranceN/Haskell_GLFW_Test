{-# LANGUAGE TemplateHaskell #-}
module Player
( Player
, tilePosition
, nextTilePosition
, isMoving

, movementDirection

, newPlayer
, flipDirections
, updatePlayer
, setPosition
, setNextPosition
, drawPlayer
) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Graphics.Rendering.OpenGL as GL

import Map

data Player = Player { _tilePosition :: (Int, Int)
                     , _nextTilePosition :: (Int, Int)
                     , _percentageThere :: GL.GLfloat
                     , _isMoving :: Bool
                     } deriving (Eq, Show)
makeLenses ''Player

newPlayer pos = Player { _tilePosition = pos
                       , _nextTilePosition = pos
                       , _percentageThere = 0
                       , _isMoving = False
                       }

updatePlayer :: State Player ()
updatePlayer = do
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

flipDirections = execState flip
  where
    flip = do
        self <- get
        let tmp = self^.tilePosition
        tilePosition .= self^.nextTilePosition
        nextTilePosition .= tmp
        percentageThere .= 1 - (self^.percentageThere)

movementDirection :: Lens Player () (Int, Int) a
movementDirection = lens diff (\player v -> ())
  where
    diff player = (nx - x, ny - y)
      where
        x = player^.tilePosition^._1
        y = player^.tilePosition^._2
        nx = player^.nextTilePosition^._1
        ny = player^.nextTilePosition^._2

drawPlayer :: StateT Player IO ()
drawPlayer = do
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
