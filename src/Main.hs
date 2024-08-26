module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Models
import Physics
import Sprites
import Render
import Data.Maybe (isJust, fromJust)
import Data.Bifunctor

main :: IO ()
main = do

  sprites <- loadSprites
  initialEnemies <- generateEnemies sprites 10

  let mainChar = Homelander (homelander sprites) (0,0) 0 (-1)

  let initialWorld = World (background sprites) (laser sprites) mainChar [] initialEnemies []

  play
    (InWindow "Haskell Boys" (bimap round round screenSize) (5000, 600)) -- Janela do jogo
    white                                         -- Cor de fundo
    60                                           -- FPS
    initialWorld
    drawWorld
    handleInput
    updateWorld

drawWorld :: World -> Picture
drawWorld world =
  let backgroundPic = backgroundSprite world
      homelanderPic = drawHomelander world
      enemiesPics = drawEnemies world
      laserPics = drawLasers world
  in pictures (backgroundPic : homelanderPic : enemiesPics ++ laserPics)

updateWorld :: Float -> World -> World
updateWorld _ world =
  let (x, y, angle, direction) = getHomelanderPosition world
      keys = getPressedKeys world
      currentProjectiles = projectiles world
      x'
        | Char 'a' `elem` keys = max (x - 10) (- fst screenSize)
        | Char 'd' `elem` keys = min (x + 10) (fst screenSize)
        | otherwise = x
      y'
        | Char 'w' `elem` keys = min (y + 10) (snd screenSize)
        | Char 's' `elem` keys = max (y - 10) (- snd screenSize)
        | otherwise = y
      direction'
        | Char 'a' `elem` keys && direction == -1 = 1
        | Char 'd' `elem` keys && direction == 1 = -1
        | otherwise = direction
      projectile' :: Maybe Projectile
      projectile'
        | MouseButton LeftButton `elem` keys = Just Projectile { pSprite = laserSprite world, pPosition = (x', y'), pDirection = direction', pRotation = angle  }
        | otherwise = Nothing
  in world {
    mainCharacter = (mainCharacter world) { position = (x', y'), rotation = angle, direction = direction' },
    projectiles = updateLasers currentProjectiles ++ [fromJust projectile' | isJust projectile']
  }
