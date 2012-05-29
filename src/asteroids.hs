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
    where player = renderLineStrip . makePlayerShape . ship $ g
          as = map (renderLineStrip . makeAsteroidShape) $ asteroids g 

makePlayerShape :: Ship -> [Vec2F]
makePlayerShape s = map (p +) ls
    where p = shipPos s
          ls = shipShape s          

makeAsteroidShape :: Asteroid -> [Vec2F]
makeAsteroidShape a = map (p +) ls
    where p = asteroidPos a
          ls = asteroidShape a          

data GameData = GameData 
    { ship :: Ship
    , asteroids :: [Asteroid]
    }
    
data Ship = Ship
    { shipPos :: Vec2F
    , shipShape :: [Vec2F]
    }
    
data Asteroid = Asteroid
    { asteroidPos :: Vec2F
    , asteroidShape :: [Vec2F]
    }


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
    

