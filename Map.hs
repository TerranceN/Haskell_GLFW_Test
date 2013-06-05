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

renderTileAtIndex :: Map -> (Tile, Int) -> IO ()
renderTileAtIndex map (tile, i) = do
    let x = (fromIntegral (i `mod` map^.rowSize)) * tileSize :: GLfloat
    let y = (fromIntegral (i `div` map^.rowSize)) * tileSize :: GLfloat
    renderTile tile x y

renderMap map = do
    GL.renderPrimitive GL.Quads $ do
        mapM_ (renderTileAtIndex map) (zip (map^.tiles) [0..((length (map^.tiles)) - 1)])
