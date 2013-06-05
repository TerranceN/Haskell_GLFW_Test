{-# LANGUAGE TemplateHaskell #-}
module Tile
( Tile(..)
, newTile
, tileType
, TileType(..)
, tileHexRadius
, renderTile
) where

import Control.Lens
import qualified Graphics.Rendering.OpenGL as GL

data TileType = NormalTile
              | WallTile
              | EmptyTile
              deriving (Eq, Show)

data Tile = Tile { _tileType :: TileType
                 } deriving (Eq, Show)
makeLenses ''Tile

tileHexRadius = 30 :: GL.GLfloat

newTile tileType = Tile { _tileType = tileType }

-- newTile index =
--     if index `rem` 2 == 0
--         then Tile { _color = GL.Color3 1 0 0 :: GL.Color3 GL.GLfloat }
--         else Tile { _color = GL.Color3 0 0 1 :: GL.Color3 GL.GLfloat }

hexPoints x y rad = do
    mapM_ (hexPoint . (\x -> x * pi/3)) (0:[1..6])
  where
    hexPoint angle = do
        GL.vertex $ GL.Vertex2 (x + rad * cos(angle)) (y + rad * sin(angle))

renderTile tile x y = do
    let shade = case tile^.tileType of
                    NormalTile -> 1.0
                    WallTile -> 0.5
                    EmptyTile -> 0.0
    GL.color $ (GL.Color3 shade shade shade :: GL.Color3 GL.GLfloat)
    GL.renderPrimitive GL.TriangleFan $ do
        GL.vertex $ GL.Vertex2 x y
        hexPoints x y tileHexRadius
    GL.color $ (GL.Color3 0 0 0 :: GL.Color3 GL.GLfloat)
    GL.renderPrimitive GL.LineStrip $ do
        hexPoints x y tileHexRadius
    -- GL.renderPrimitive GL.Quads $ do
    --     GL.vertex $ GL.Vertex2 x y
    --     GL.vertex $ GL.Vertex2 (x + tileSize) y
    --     GL.vertex $ GL.Vertex2 (x + tileSize) (y + tileSize)
    --     GL.vertex $ GL.Vertex2 x (y + tileSize)
    -- GL.color $ (GL.Color3 0 0 0 :: GL.Color3 GL.GLfloat)
    -- GL.renderPrimitive GL.LineStrip $ do
    --     GL.vertex $ GL.Vertex2 x y
    --     GL.vertex $ GL.Vertex2 (x + tileSize) y
    --     GL.vertex $ GL.Vertex2 (x + tileSize) (y + tileSize)
    --     GL.vertex $ GL.Vertex2 x (y + tileSize)
    --     GL.vertex $ GL.Vertex2 x y
