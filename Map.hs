{-# LANGUAGE TemplateHaskell #-}
module Map 
( Map(..)
, newMap
, tiles
, rowSize
, renderMap
, gridToMap
, tileAt
, tileIsLower
) where

import Control.Lens
import Graphics.Rendering.OpenGL as GL hiding (get)

import Tile

data Map = Map { _tiles :: [Tile] 
               , _mapSize :: Int
               , _rowSize :: Int
               } deriving (Eq, Show)
makeLenses ''Map

newMap = Map { _tiles = map (\x -> if (x `mod` 7 == 0) then newTile WallTile else newTile NormalTile) [1..defaultMapSize]
             , _mapSize = defaultMapSize
             , _rowSize = 11
             }
  where
    defaultMapSize = 110

tileAt (x, y) gameMap =
    if locationWithinBounds
        then Just $ (gameMap^.tiles) !! index
        else Nothing
  where
    index = (y * (gameMap^.rowSize) + x)
    mapWidth = gameMap^.rowSize
    mapHeight = gameMap^.mapSize `div` gameMap^.rowSize
    locationWithinBounds =
        x >= 0 &&
        y >= 0 &&
        x < mapWidth &&
        y < mapHeight

tileIsLower x = x `mod` 2 == 1

gridToMap :: (Int, Int) -> (GL.GLfloat, GL.GLfloat)
gridToMap (x, y) = (realX, realY)
  where
    realX = (fromIntegral x) * offsetX :: GLfloat
    realY = (fromIntegral y) * hexToBottom * 2 + offsetY :: GLfloat
    offsetX = 3 / 2 * tileHexRadius
    offsetY = if (tileIsLower x) then (hexToBottom) else 0
    hexToBottom = cos(pi/6) * tileHexRadius

renderTileAtIndex :: Map -> (Tile, Int) -> IO ()
renderTileAtIndex map (tile, i) = do
    let (gridY, gridX) = (i `divMod` (map^.rowSize))
    let (x, y) = gridToMap (gridX, gridY)
    renderTile tile x y shade
  where
    shade =
        case tile^.tileType of
            NormalTile -> 1.0
            WallTile -> 0.5
            EmptyTile -> 0.0

renderMap map = do
    mapM_ (renderTileAtIndex map) (zip (map^.tiles) [0..((length (map^.tiles)) - 1)])
