{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.UI.Engine 
       ( Engine
       , WindowSpec(..)
       , executeEngine
       , startFrame
       , renderLineList
       , renderString
       , isLMBPressed
       , getMousePos
       )  where


import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import qualified Data.StateVar as SV
import Data.StateVar ( ($=) )

import Data.Vec.Packed
import Data.IORef
import Control.Monad
import Control.Monad.Trans

newtype Engine a = Engine { runEngine :: IO a }
              deriving (Monad, MonadIO)
                       
data WindowSpec = WindowSpec { windowWidth :: Int
                             , windowHeight :: Int 
                             , windowTitle :: String 
                             }


executeEngine :: WindowSpec -> Engine () -> IO ()
executeEngine spec innerLoop = do
  initializeGLFW
  
  -- open window
  let windowSize = GL.Size (fromIntegral . windowWidth $ spec) 
                           (fromIntegral . windowHeight $ spec)
  GLFW.openWindow windowSize [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= windowTitle spec
  
    -- This must happen after a window has been opened.
  initializeGL
  
  -- set the color to clear background
  GL.clearColor $= GL.Color4 0 0 0 0
  
  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  projection_ref <- newIORef ((Vec2F (-1) (-1)), (Vec2F 1 1))
  GLFW.windowSizeCallback $= resizeWindow projection_ref
  -- invoke the active drawing loop
  runEngine innerLoop
  -- finish up
  GLFW.closeWindow
  GLFW.terminate

initializeGLFW :: IO ()
initializeGLFW = do
  GLFW.initialize
  GLFW.swapInterval $= 1
  GLFW.enableSpecial GLFW.StickyKey

initializeGL :: IO ()
initializeGL = do
  GL.shadeModel $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth $= 1.5

resizeWindow :: IORef (Vec2F, Vec2F) -> GL.Size -> IO ()
resizeWindow projection_ref size@(GL.Size w h) = do
  GL.viewport $= (GL.Position 0 0, size)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  ((Vec2F x0 y0), (Vec2F x1 y1)) <- readIORef projection_ref
  GL.ortho2D (realToFrac x0) (realToFrac x1) (realToFrac y1) (realToFrac y0)

isLMBPressed :: Engine Bool
isLMBPressed = Engine $ do
  b <- GLFW.getMouseButton $ GLFW.ButtonLeft
  return (b == GLFW.Press)

getMousePos :: Engine Vec2I
getMousePos = Engine $ do
      (GL.Position x y) <- SV.get $ GLFW.mousePos 
      return $ Vec2I (fromIntegral x) (fromIntegral y)
      
startFrame :: Engine Bool
startFrame = Engine $ do
  GLFW.sleep 0.001
  GLFW.swapBuffers
  GL.clear [GL.ColorBuffer]
  
  esc_pressed <- GLFW.getKey GLFW.ESC
  window_opened <- SV.get $ GLFW.windowParam GLFW.Opened
  
  return $ (window_opened /= 0) && (esc_pressed /= GLFW.Press)
  
  
renderLineList :: [Vec2F] -> Engine ()
renderLineList lines = Engine $ do
  GL.color $ color3 1 0 0
  GL.renderPrimitive GL.Lines $ mapM_  point2vertex lines
  where 
    point2vertex (Vec2F x y) = GL.vertex $ vertex3 (realToFrac x) (realToFrac y) 0
 
renderString :: Vec2F -> String -> Engine ()
renderString (Vec2F x y) str = Engine . GL.preservingMatrix $ do    
  GL.translate (GL.Vector3 (realToFrac x) (realToFrac y + 16) (0::GL.GLfloat))
  GL.scale 1 (-1) (1::GL.GLfloat)
  GLFW.renderString GLFW.Fixed8x16 str

-- This exists just to fix type.
vertex3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Vertex3 GL.GLfloat
vertex3 = GL.Vertex3
 
-- This exists just to fix type.
color3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Color3 GL.GLfloat
color3 = GL.Color3