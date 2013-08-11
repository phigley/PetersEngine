{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

module Graphics.UI.Engine 
       ( Engine
       , Render
       , WindowSpec(..)
       , executeEngine
       , renderLineList
       , renderLineStrip
       , isLMBPressed
       , getMousePos
       )  where


import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Graphics.Rendering.OpenGL ( ($=) )

import Data.Vect.Float
import Data.Vect.Float.OpenGL ()

import Data.IORef
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Concurrent

data EngineData = EngineData 
                  { projection :: IORef (Vec2, Vec2)
                  , window :: GLFW.Window
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

doOpenWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
doOpenWindow w h title onSucess = do
    maybeWin <- GLFW.createWindow w h title Nothing Nothing
    case maybeWin of 
       Nothing  -> error "Failed to open window."
       Just win -> do
          GLFW.makeContextCurrent $ Just win
          onSucess win 
          GLFW.setWindowShouldClose win True

executeEngine :: forall gameState . 
                WindowSpec   -- | initial window configuration
                -> gameState  -- | initial game state
                -> (gameState -> Engine gameState) -- | update function
                -> (gameState -> Render) -- | render function
                -> IO ()
executeEngine spec initialGameState executeFrame renderFrame =   initializeGLFW $ do
  -- open window
  let width = fromIntegral . windowWidth $ spec 
      height = fromIntegral . windowHeight $ spec
      title = windowTitle spec
                         
  doOpenWindow width height title $ \win -> do
      
        -- This must happen after a window has been opened.
      initializeGL
      
      -- set the color to clear background
      GL.clearColor $= GL.Color4 0 0 0 0
      
      -- set 2D orthogonal view inside windowSizeCallback because
      -- any change to the Window size should result in different
      -- OpenGL Viewport.
      projection_ref <- newIORef (Vec2 (-1) (-1), Vec2 1 1)
      GLFW.setWindowSizeCallback win . Just $ resizeWindow projection_ref
      
      let engineData = EngineData 
                { projection = projection_ref 
                , window = win
                }
          
      -- invoke the active drawing loop
      loop engineData executeFrame renderFrame initialGameState
      
  GLFW.terminate


loop :: forall gameState . 
       EngineData
       -> (gameState -> Engine gameState) 
       -> (gameState -> Render) 
       -> gameState 
       -> IO ()
loop engineData executeFrame renderFrame currentGameState = do
  continue <- startFrame . window $ engineData
  when continue $ do
    mapM_ runRenderShape $ renderFrame currentGameState
    newGameState <- runReaderT (runEngine $ executeFrame currentGameState) engineData
    loop engineData executeFrame renderFrame newGameState


initializeGLFW :: IO () -> IO ()
initializeGLFW onSucess = do
  success <- GLFW.init
  if not success
   then putStrLn "Failed to initialize GLFW"
   else do 
     onSucess
     GLFW.terminate
     
initializeGL :: IO ()
initializeGL = do
  GL.shadeModel $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth $= 1.5

resizeWindow :: IORef (Vec2, Vec2) -> GLFW.Window -> Int -> Int -> IO ()
resizeWindow projection_ref _ w h = do
  let size = GL.Size (fromIntegral w) (fromIntegral h)
  GL.viewport $= (GL.Position 0 0, size)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  (Vec2 x0 y0, Vec2 x1 y1) <- readIORef projection_ref
  GL.ortho2D (realToFrac x0) (realToFrac x1) (realToFrac y1) (realToFrac y0)

isLMBPressed :: Engine Bool
isLMBPressed = Engine $ do
               win <- asks window
               mb_state <- liftIO $ GLFW.getMouseButton win GLFW.MouseButton'1
               return $ mb_state == GLFW.MouseButtonState'Pressed

getMousePos :: Engine Vec2
getMousePos = Engine $ do
      engineData <- ask
      let win = window engineData
      (x, y) <- liftIO $ GLFW.getCursorPos win
      (wx, wy) <- liftIO $ GLFW.getWindowSize win
      ( Vec2 minpx minpy, Vec2 maxpx maxpy ) <- liftIO . readIORef $ projection engineData
      let xf = minpx + (maxpx - minpx) *  realToFrac x /  fromIntegral wx
          yf = maxpy + (minpy - maxpy) *  realToFrac y /  fromIntegral wy
      return $ Vec2 xf yf
      
startFrame :: GLFW.Window -> IO Bool
startFrame win = do
  threadDelay 1
  GLFW.swapBuffers win
  GLFW.pollEvents
  GL.clear [GL.ColorBuffer]
  
  esc_state <- GLFW.getKey win GLFW.Key'Escape
  window_close <- GLFW.windowShouldClose win
  
  return $ not window_close && esc_state /= GLFW.KeyState'Pressed
  
  
renderLineList :: [Vec2] -> RenderShape
renderLineList ls = RenderShape $ do
  GL.color $ color3 1 0 0
  GL.renderPrimitive GL.Lines $ mapM_  GL.vertex ls


 
renderLineStrip :: [Vec2] -> RenderShape
renderLineStrip linestrip = RenderShape $ do
  GL.color $ color3 1 0 0
  GL.renderPrimitive GL.LineStrip $ mapM_  GL.vertex linestrip

-- This exists just to fix type.
color3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Color3 GL.GLfloat
color3 = GL.Color3

--vec2IToVec2 :: Vec2I -> Vec2
--vec2IToVec2 (Vec2I xi yi) = Vec2 (fromIntegral xi) (fromIntegral yi)

--glSizeToVec2 :: GL.Size -> Vec2
--glSizeToVec2 (GL.Size h w) = Vec2 (fromIntegral h) (fromIntegral w)