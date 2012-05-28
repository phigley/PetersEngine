module Main where

import Data.Vec.Packed

import Graphics.UI.Engine

windowSpec :: WindowSpec
windowSpec = WindowSpec 
    { windowWidth = 800
    , windowHeight = 800
    , windowTitle = "Asteroids"
    }
    
main :: IO ()
main = executeEngine windowSpec initialGameState return renderGameData

renderGameData :: GameData -> Render
renderGameData g = 
    player : as
    where player = renderLineStrip . makePlayerShape . ship $ g
          as = replicate 3 (renderLineStrip [])

makePlayerShape :: Ship -> [Vec2F]
makePlayerShape s = map (p +) ls
    where p = shipPos s
          ls = shipShape s          

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
    