module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

import Graphics.UI.Engine
import Data.Vec.Packed

type Point = Vec2F
type PointList = [Point]

type ModelT = StateT GameState Engine

data GameState = GameState 
                 { lineList :: PointList
                 , inputHandler :: ModelT ()
                 }

initialGameState :: GameState
initialGameState = GameState { lineList = []
                             , inputHandler = waitForPress
                             }

updateFrame :: GameState -> Engine GameState
updateFrame gameState = do
  execStateT (inputHandler gameState) gameState

renderFrame :: GameState -> Render
renderFrame gameState = [ renderLineList $ lineList gameState ]

  

      
startLine :: Point -> PointList -> PointList
startLine p = (p:) .(p:)

updateEndPoint :: Point -> PointList -> PointList
updateEndPoint p' = (p':) . tail

waitForPress :: ModelT ()
waitForPress = do
  b <- lift isLMBPressed
  when b  $ do 
    -- when left mouse button is pressed, add the point
    -- to lines and switch to waitForRelease action.
    mousePos <- lift getMousePos
    liftIO $ print mousePos
    gs <- get
    let gs' = gs { lineList = (startLine mousePos) (lineList gs)
                 , inputHandler = waitForRelease
                 }
    put gs'

      
waitForRelease :: ModelT ()
waitForRelease = do
  -- Keep track of mouse movement while waiting for button 
  -- release
  gs <- get
  mousePos <- lift getMousePos 
  put $ gs { lineList = updateEndPoint mousePos (lineList gs) }
  b <- lift isLMBPressed
  when (not b) $ do
    gs <- get
    put $ gs { inputHandler = waitForPress }


windowSpec :: WindowSpec
windowSpec = WindowSpec { windowWidth = 400
                        , windowHeight = 400
                        , windowTitle = "Simple Engine"
                        }
main :: IO ()
main =   executeEngine windowSpec initialGameState updateFrame renderFrame
