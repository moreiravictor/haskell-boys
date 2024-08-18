module Models where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Homelander = Homelander
  {
    sprite        :: Picture,
    position      :: (Float, Float),
    rotation      :: Float,
    direction     :: Float
  }

data Enemy = Enemy
  {
    enemySprite        :: Picture,
    enemyPosition      :: (Float, Float),
    enemyDirection     :: Float
  }

-- generateEnemy :: IO Enemy


data World = World
  {  backgroundSprite :: Picture,
     mainCharacter    :: Homelander,
     pressedKeys      :: [Key],
     enemies          :: [Enemy]
  }

getHomelanderPosition :: World -> (Float, Float, Float, Float)
getHomelanderPosition world =
  let (x, y) = position (mainCharacter world)
      angle = rotation (mainCharacter world)
      direction' = direction (mainCharacter world)
  in (x, y, angle, direction')

setHomelanderPosition :: (Float, Float) -> World -> World
setHomelanderPosition newPos world =
  let oldHomelander = mainCharacter world
      newHomelander = oldHomelander { position = newPos }
  in world { mainCharacter = newHomelander }

addPressedKey :: Key -> World -> World
addPressedKey key world = world { pressedKeys = key : pressedKeys world }

removePressedKey :: Key -> World -> World
removePressedKey key world = world { pressedKeys = filter (/= key) (pressedKeys world) }

getPressedKeys :: World -> [Key]
getPressedKeys = pressedKeys
