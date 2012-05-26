{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

module Graphics.UI.Engine 
       ( Engine
       , Render
       , WindowSpec(..)
       , executeEngine
       , renderLineList
       , renderLineStrip
--       , renderString
       , isLMBPressed
       , getMousePos
       )  where


import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Graphics.Rendering.OpenGL ( ($=) )

import Data.Vec.Packed
import Data.IORef
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader

data EngineData = EngineData 
                  { projection :: IORef (Vec2F, Vec2F)
                  }

type EngineT = ReaderT EngineData IO

newtype Engine a = Engine { runEngine :: EngineT a }
              deriving (Monad, MonadIO)
                       
newtype RenderShape = RenderShape { runRenderShape :: IO () }
type Render = [RenderShape]
                 
data WindowSpec = WindowSpec { windowWidth :: Int
                             , windowHeight :: Int 
                             , windowTitle :: String 
                             }


executeEngine :: forall gameState . 
                WindowSpec   -- | initial window configuration
                -> gameState  -- | initial game state
                -> (gameState -> Engine gameState) -- | update function
                -> (gameState -> Render) -- | render function
                -> IO ()
executeEngine spec initialGameState executeFrame renderFrame = do
  initializeGLFW
  
  -- open window
  let width = fromIntegral . windowWidth $ spec 
      height = fromIntegral . windowHeight $ spec
      displayOptions = GLFW.defaultDisplayOptions 
                         { GLFW.displayOptions_width = width
                         , GLFW.displayOptions_height = height
                         , GLFW.displayOptions_numAlphaBits = 8
                         }
  GLFW.openWindow displayOptions
  GLFW.setWindowTitle $ windowTitle spec
  
    -- This must happen after a window has been opened.
  initializeGL
  
  -- set the color to clear background
  GL.clearColor $= GL.Color4 0 0 0 0
  
  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  projection_ref <- newIORef (Vec2F (-1) (-1), Vec2F 1 1)
  GLFW.setWindowSizeCallback $ resizeWindow projection_ref
  
  let engineData = EngineData { projection = projection_ref }
      
  -- invoke the active drawing loop
  loop engineData executeFrame renderFrame initialGameState
  
  -- finish up
  GLFW.closeWindow
  GLFW.terminate


loop :: forall gameState . 
       EngineData
       -> (gameState -> Engine gameState) 
       -> (gameState -> Render) 
       -> gameState 
       -> IO ()
loop engineData executeFrame renderFrame currentGameState = do
  continue <- startFrame
  when continue $ do
    mapM_ runRenderShape $ renderFrame currentGameState
    newGameState <- runReaderT (runEngine $ executeFrame currentGameState) engineData
    loop engineData executeFrame renderFrame newGameState


initializeGLFW :: IO ()
initializeGLFW = do
  GLFW.initialize
  GLFW.setWindowBufferSwapInterval 1

initializeGL :: IO ()
initializeGL = do
  GL.shadeModel $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth $= 1.5

resizeWindow :: IORef (Vec2F, Vec2F) -> Int -> Int -> IO ()
resizeWindow projection_ref w h = do
  let size = GL.Size (fromIntegral w) (fromIntegral h)
  GL.viewport $= (GL.Position 0 0, size)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  (Vec2F x0 y0, Vec2F x1 y1) <- readIORef projection_ref
  GL.ortho2D (realToFrac x0) (realToFrac x1) (realToFrac y1) (realToFrac y0)

isLMBPressed :: Engine Bool
isLMBPressed = Engine . liftIO 
               $ GLFW.mouseButtonIsPressed GLFW.MouseButton0

getMousePos :: Engine Vec2F
getMousePos = Engine $ do
      (x, y) <- liftIO GLFW.getMousePosition 
      (wx, wy) <- liftIO GLFW.getWindowDimensions
      engineData <- ask
      ( Vec2F minpx minpy, Vec2F maxpx maxpy ) <- liftIO . readIORef $ projection engineData
      let xf = minpx + (maxpx - minpx)* fromIntegral x / fromIntegral wx
          yf = minpy + (maxpy - minpy)* fromIntegral y / fromIntegral wy
      return $ Vec2F xf yf
      
startFrame :: IO Bool
startFrame = do
  GLFW.sleep 0.001
  GLFW.swapBuffers
  GL.clear [GL.ColorBuffer]
  
  esc_pressed <- GLFW.keyIsPressed GLFW.KeyEsc
  window_opened <- GLFW.windowIsOpen
  
  return $ window_opened && not esc_pressed
  
  
renderLineList :: [Vec2F] -> RenderShape
renderLineList lines = RenderShape $ do
  GL.color $ color3 1 0 0
  GL.renderPrimitive GL.Lines $ mapM_  point2vertex lines
  where 
    point2vertex (Vec2F x y) = GL.vertex $ vertex3 (realToFrac x) (realToFrac y) 0
 
renderLineStrip :: [Vec2F] -> RenderShape
renderLineStrip linestrip = RenderShape $ do
  GL.color $ color3 1 0 0
  GL.renderPrimitive GL.LineStrip $ mapM_  point2vertex linestrip
  where 
    point2vertex (Vec2F x y) = GL.vertex $ vertex3 (realToFrac x) (realToFrac y) 0

--renderString :: Vec2F -> String -> Engine ()
--renderString (Vec2F x y) str = Engine . liftIO . GL.preservingMatrix $ do    
--  GL.translate (GL.Vector3 (realToFrac x) (realToFrac y) (0::GL.GLfloat))
--  GL.scale 1 (-1) (1::GL.GLfloat)
--  GLFW.renderString GLFW.Fixed8x16 str

-- This exists just to fix type.
vertex3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Vertex3 GL.GLfloat
vertex3 = GL.Vertex3
 
-- This exists just to fix type.
color3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Color3 GL.GLfloat
color3 = GL.Color3

vec2IToVec2F :: Vec2I -> Vec2F
vec2IToVec2F (Vec2I xi yi) = Vec2F (fromIntegral xi) (fromIntegral yi)

glSizeToVec2F :: GL.Size -> Vec2F
glSizeToVec2F (GL.Size h w) = Vec2F (fromIntegral h) (fromIntegral w)