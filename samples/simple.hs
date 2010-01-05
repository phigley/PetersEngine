module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

import Graphics.UI.Engine
import Data.Vec.Packed

type Point = Vec2F
type PointList = [Point]

type ModelT = StateT PointList Engine
data Action m = Action { executeAction :: m (Action m) }

loop ::Action ModelT -> ModelT ()
loop (Action action) = do
  continue <- lift startFrame
  when continue $ do
    -- draw the entire screen
    get >>= lift . renderLineList
    lift $ renderString (Vec2F 0 0) "Hello"
    -- perform action
    nextAction <- action
    
    loop nextAction

      
startLine :: Point -> PointList -> PointList
startLine p = (p:) .(p:)

updateEndPoint :: Point -> PointList -> PointList
updateEndPoint p' = (p':) . tail

waitForPress :: Action ModelT
waitForPress = Action $ do
  b <- lift isLMBPressed
  if (not b)
    then return waitForPress
    else do
      -- when left mouse button is pressed, add the point
      -- to lines and switch to waitForRelease action.
      (lift getMousePos) >>= (modify . startLine . vec2IToVec2F)
      mousePos <- lift getMousePos
      liftIO $ print mousePos
      return waitForRelease
 
vec2IToVec2F :: Vec2I -> Vec2F
vec2IToVec2F (Vec2I xi yi) = Vec2F (fromIntegral xi) (fromIntegral yi)

waitForRelease :: Action ModelT
waitForRelease = Action $ do
  -- Keep track of mouse movement while waiting for button 
  -- release
  (lift getMousePos) >>= (modify . updateEndPoint .vec2IToVec2F)
  b <- lift isLMBPressed
  if (not b)
    then return waitForPress
    else return waitForRelease


engine :: Engine ()
engine = evalStateT (loop waitForPress) []

main :: IO ()
main =   executeEngine engine
