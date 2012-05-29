module Main where

import Data.Vec.Packed
import Control.Monad
import Control.Monad.Random

import Graphics.UI.Engine

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

class Shape a where
    pos :: a -> Vec2F
    shape :: a -> [Vec2F]
    
makeTransposedShape :: Shape a => a -> [Vec2F]
makeTransposedShape s = map (p +) ls
    where p = pos s
          ls = shape s


data GameData = GameData 
    { ship :: Ship
    , asteroids :: [Asteroid]
    }
    
data Ship = Ship
    { shipPos :: Vec2F
    , shipShape :: [Vec2F]
    }

instance Shape Ship where
    pos = shipPos
    shape = shipShape
        
data Asteroid = Asteroid
    { asteroidPos :: Vec2F
    , asteroidShape :: [Vec2F]
    }

instance Shape Asteroid where
    pos = asteroidPos
    shape = asteroidShape

initialPlayerShip :: Ship
initialPlayerShip = Ship
    { shipPos = Vec2F 0 0
    , shipShape = 
       [ Vec2F   0    0.05
       , Vec2F  0.025   (-0.0125)
       , Vec2F   0     (-0.006)
       , Vec2F (-0.025) (-0.0125)
       , Vec2F   0    0.05
       ]
     }
       
initialGameState :: GameData
initialGameState = GameData
    { ship = initialPlayerShip
    , asteroids = []
    }
    

createAsteroidShape :: (RandomGen g) => Rand g [Vec2F]
createAsteroidShape = do
    sides <- getRandomR (3,12)
    radii <- replicateM sides $ getRandomR (0.1, 0.25)
    let anglevariance = pi / fromIntegral sides
        addVariance :: Float -> (RandomGen g) => Rand g Float
        addVariance a = do av <- getRandomR (-anglevariance, anglevariance)
                           return (av + a)
    angles <- mapM addVariance $ take sides $ iterate ((2 * pi / fromIntegral sides) +) 0
    let toCoord a r = Vec2F (r * cos a) (r * sin a)
        result = zipWith toCoord angles radii ++ [head result]
    return result
    

createAsteroid :: IO Asteroid
createAsteroid = do
    s <- evalRandIO createAsteroidShape
    px <- evalRandIO $ getRandomR (-1, 1)
    py <- evalRandIO $ getRandomR (-1, 1)
    return Asteroid {asteroidPos = Vec2F px py, asteroidShape = s}
    

