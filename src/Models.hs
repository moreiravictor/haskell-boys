module Models where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomRIO)
import Sprites

screenSize :: (Float, Float)
screenSize = (1600, 900)

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
    pRotation      :: Float,
    pDirection     :: Float
  } deriving Eq

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
      let (width, height) = screenSize
      let halfWidth = width / 2
      let halfHeight = height / 2

      let spritesArr   = getAllSprites sprites
      randomSpriteIndex <- randomRIO (0, length spritesArr - 1)
      let sprite' = spritesArr !! randomSpriteIndex

      borderSide <- randomRIO (1 :: Int, 4)
      (x, y) <- case borderSide of
        1 -> do -- Top border
          xPos <- randomRIO (-halfWidth, halfWidth)
          return (xPos, halfHeight + 100)  -- Slightly outside the top
        2 -> do -- Bottom border
          xPos <- randomRIO (-halfWidth, halfWidth)
          return (xPos, -(halfHeight + 100))  -- Slightly outside the bottom
        3 -> do -- Left border
          yPos <- randomRIO (-halfHeight, halfHeight)
          return (-(halfWidth + 100), yPos)  -- Slightly outside the left
        _ -> do -- Right border
          yPos <- randomRIO (-halfHeight, halfHeight)
          return (halfWidth + 100, yPos)  -- Slightly outside the right

      return $ Enemy sprite' (x, y) 1

data World = World
  {  backgroundSprite :: Picture,
     laserSprite      :: Picture,
     mainCharacter    :: Homelander,
     pressedKeys      :: [Key],
     enemies          :: [Enemy],
     projectiles      :: [Projectile]
  }

getProjectiles :: World -> [Projectile]
getProjectiles = projectiles

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
