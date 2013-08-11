module Main where

import Data.Vect.Float
import Control.Monad
import Control.Monad.Random

import Graphics.UI.Engine

import GameData

windowSpec :: WindowSpec
windowSpec = WindowSpec 
    { windowWidth = 800
    , windowHeight = 800
    , windowTitle = "Asteroids"
    }
    
main :: IO ()
main = do
    ats <- replicateM 3 createAsteroid
    let gs = initialGameState { asteroids = ats }
    executeEngine windowSpec gs return renderGameData

renderGameData :: GameData -> Render
renderGameData g = 
    player : as
    where player = renderLineStrip . makeTransposedShape . ship $ g
          as = map (renderLineStrip . makeTransposedShape) . asteroids $ g 
    
makeTransposedShape :: Shape a => a -> [Vec2]
makeTransposedShape s = map (p &+) ls
    where p = pos s
          ls = shape s


initialPlayerShip :: Ship
initialPlayerShip = Ship
    { shipPos = Vec2 0 0
    , shipShape = 
       [ Vec2   0    0.05
       , Vec2  0.025   (-0.0125)
       , Vec2   0     (-0.006)
       , Vec2 (-0.025) (-0.0125)
       , Vec2   0    0.05
       ]
     }
       
initialGameState :: GameData
initialGameState = GameData
    { ship = initialPlayerShip
    , asteroids = []
    }
    

createAsteroidShape :: (RandomGen g) => Rand g [Vec2]
createAsteroidShape = do
    sides <- getRandomR (3,12)
    radii <- replicateM sides $ getRandomR (0.1, 0.25)
    let anglevariance = pi / fromIntegral sides
        addVariance :: (RandomGen g) => Float -> Rand g Float
        addVariance a = do av <- getRandomR (-anglevariance, anglevariance)
                           return (av + a)
    angles <- mapM addVariance $ take sides $ iterate ((2 * pi / fromIntegral sides) +) 0
    let toCoord a r = Vec2 (r * cos a) (r * sin a)
        result = zipWith toCoord angles radii ++ [head result]
    return result
    

createAsteroid :: IO Asteroid
createAsteroid = do
    s <- evalRandIO createAsteroidShape
    px <- evalRandIO $ getRandomR (-1, 1)
    py <- evalRandIO $ getRandomR (-1, 1)
    return Asteroid {asteroidPos = Vec2 px py, asteroidShape = s}
    

