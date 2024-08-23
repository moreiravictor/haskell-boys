module Models where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomRIO)
import Sprites

data Homelander = Homelander
  {
    sprite        :: Picture,
    position      :: (Float, Float),
    rotation      :: Float,
    direction     :: Float
  }

data Projectile = Projectile
  {
    pSprite        :: Picture,
    pPosition      :: (Float, Float),
    pDirection     :: Float
  }

data Enemy = Enemy
  {
    eSprite        :: Picture,
    ePosition      :: (Float, Float),
    eDirection     :: Float
  }

generateEnemies :: GameSprites -> Int -> IO [Enemy]
generateEnemies sprites n = mapM (const generateEnemy) [1..n]
  where
    generateEnemy = do
      let spritesArr   = getAllSprites sprites
      randomSpriteIndex <- randomRIO (0, length spritesArr - 1)
      let sprite' = spritesArr !! randomSpriteIndex
      x <- randomRIO (-750, 750)
      y <- randomRIO (-410, 410)
      return $ Enemy sprite' (x, y) 1

data World = World
  {  backgroundSprite :: Picture,
     laserSprite      :: Picture,
     mainCharacter    :: Homelander,
     pressedKeys      :: [Key],
     enemies          :: [Enemy],
     projectiles      :: [Projectile]
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
