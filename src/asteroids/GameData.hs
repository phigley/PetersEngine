
module GameData where

import Data.Vect.Float

class Shape a where
    pos :: a -> Vec2
    shape :: a -> [Vec2]

data GameData = GameData 
    { ship :: Ship
    , asteroids :: [Asteroid]
    }
    
data Ship = Ship
    { shipPos :: Vec2
    , shipShape :: [Vec2]
    }

instance Shape Ship where
    pos = shipPos
    shape = shipShape
        
data Asteroid = Asteroid
    { asteroidPos :: Vec2
    , asteroidShape :: [Vec2]
    }

instance Shape Asteroid where
    pos = asteroidPos
    shape = asteroidShape
