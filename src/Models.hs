module Models where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Homelander = Homelander
  {
    sprite        :: Picture,
    position      :: (Float, Float)
  }

data World = World
  {  backgroundSprite :: Picture,
     mainCharacter :: Homelander,
     pressedKeys   :: [Key]
  }

getHomelanderPosition :: World -> (Float, Float)
getHomelanderPosition world = position (mainCharacter world)

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
