module Models where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Sprites
import Control.Monad.State

screenSize :: (Float, Float)
screenSize = (1600, 900)

screenPosition :: (Int, Int)
screenPosition = (0, 0)

data Homelander = Homelander
  {
    sprite        :: Picture,
    position      :: (Float, Float),
    rotation      :: Float,
    direction     :: Float,
    blink         :: (Bool, Float)
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
    eDirection     :: Float,
    eInitialBorder :: Int
  }

data GameStats = GameStats
  {
    kills :: Int,
    life  :: Int
  }

initialStats :: GameStats
initialStats = GameStats { kills = 0, life = 3 }

data World = World
  {
      gameState         :: GameState,
      stats             :: GameStats,
      time              :: Float,
      gameSprites       :: GameSprites,
      mainCharacter     :: Homelander,
      pressedKeys       :: [Key],
      enemies           :: [Enemy],
      enemiesAmount     :: Int,
      projectiles       :: [Projectile]
  }

type GameMonad = State World

getHomelanderPosition :: World -> (Float, Float, Float, Float)
getHomelanderPosition world =
  let (x, y) = position (mainCharacter world)
      angle = rotation (mainCharacter world)
      direction' = direction (mainCharacter world)
  in (x, y, angle, direction')

addPressedKey :: Key -> World -> World
addPressedKey key world = world { pressedKeys = key : pressedKeys world }

removePressedKey :: Key -> World -> World
removePressedKey key world = world { pressedKeys = filter (/= key) (pressedKeys world) }

data GameState = Menu | Stage1 | Stage2 | Stage3 | GameOver | Win

stage1EnemiesAmount :: Int
stage1EnemiesAmount = 15

stage2EnemiesAmount :: Int
stage2EnemiesAmount = 20

stage3EnemiesAmount :: Int
stage3EnemiesAmount = 25