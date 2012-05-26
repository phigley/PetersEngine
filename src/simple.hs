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

box :: PointList
box = [ Vec2F  (0.25)  (0.25)
      , Vec2F (-0.25)  (0.25)
      , Vec2F (-0.25) (-0.25)
      , Vec2F  (0.25) (-0.25) 
      , head box ]

windowSpec :: WindowSpec
windowSpec = WindowSpec { windowWidth = 400
                        , windowHeight = 400
                        , windowTitle = "Simple Engine"
                        }
             
renderBox :: PointList -> Render
renderBox ps = [ renderLineStrip ps ]

main :: IO ()
main =   executeEngine windowSpec box (\gs -> return gs) renderBox
