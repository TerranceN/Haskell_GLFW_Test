{-# LANGUAGE TemplateHaskell #-}
module Map 
( Map(..)
, newMap
, tiles
, rowSize
, renderMap
) where

import Control.Lens
import Graphics.Rendering.OpenGL as GL hiding (get)

import Tile

data Map = Map { _tiles :: [Tile] 
               , _rowSize :: Int
               } deriving (Eq, Show)
makeLenses ''Map

newMap = Map { _tiles = map (\x -> if (x `mod` 7 == 0) then newTile WallTile else newTile NormalTile) [1..100]
             , _rowSize = 10
             }

gridToMap :: (Int, Int) -> (GL.GLfloat, GL.GLfloat)
gridToMap (x, y) = (realX, realY)
  where
    realX = (fromIntegral x) * offsetX :: GLfloat
    realY = (fromIntegral y) * hexToBottom * 2 + offsetY :: GLfloat
    offsetX = 3 / 2 * tileHexRadius
    offsetY = if (x `mod` 2 == 1) then (hexToBottom) else 0
    hexToBottom = cos(pi/6) * tileHexRadius

renderTileAtIndex :: Map -> (Tile, Int) -> IO ()
renderTileAtIndex map (tile, i) = do
    let (x, y) = gridToMap (i `divMod` (map^.rowSize))
    renderTile tile x y

renderMap map = do
    GL.renderPrimitive GL.Quads $ do
        mapM_ (renderTileAtIndex map) (zip (map^.tiles) [0..((length (map^.tiles)) - 1)])
