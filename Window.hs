module Window where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))

windowSize :: (Int, Int)
windowSize = (400, 400)

tupleToGLSize :: (Int, Int) -> GL.Size
tupleToGLSize (x, y) = GL.Size (fromIntegral x) (fromIntegral y)

initWindow = do
    GLFW.initialize
    -- open window
    GLFW.openWindowHint GLFW.NoResize True
    GLFW.openWindow (tupleToGLSize windowSize) [GLFW.DisplayAlphaBits 8] GLFW.Window
    GLFW.windowTitle $= "GLFW Demo"
    GL.shadeModel    $= GL.Smooth
    -- enable antialiasing
    GL.lineSmooth $= GL.Enabled
    GL.blend      $= GL.Enabled
    GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.lineWidth  $= 1.5
    -- set the color to clear background
    GL.clearColor $= GL.Color4 0 0 0 0

    -- set 2D orthogonal view inside windowSizeCallback because
    -- any change to the Window size should result in different
    -- OpenGL Viewport.
    GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
      do
        GL.viewport   $= (GL.Position 0 0, size)
        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
    -- keep all line strokes as a list of points in an IORef

closeWindow = do
    GLFW.closeWindow
    GLFW.terminate
