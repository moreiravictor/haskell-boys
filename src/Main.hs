module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Models
import Physics
import Sprites

main :: IO ()
main = do

  sprites <- loadSprites
  enemies <- generateEnemies sprites 20

  let mainCharacter = Homelander (homelander sprites) (0,0) 0 (-1)

  let initialWorld = World (background sprites) mainCharacter [] enemies

  play
    (InWindow "Haskell Boys" (1600, 900) (0, 0)) -- Janela do jogo
    white                                         -- Cor de fundo
    60                                           -- FPS
    initialWorld
    drawWorld
    handleInput
    updateWorld

drawWorld :: World -> Picture
drawWorld world =
  let bg = backgroundSprite world
      (x, y) = position (mainCharacter world)
      charSprite = sprite (mainCharacter world)
      orientation = direction (mainCharacter world)
      rotatedSprite = rotate (rotation (mainCharacter world)) charSprite
      scaledSprite = scale orientation 1 rotatedSprite
      enemiesPics = map (\enemy -> uncurry translate (ePosition enemy) (eSprite enemy)) (enemies world)
  in pictures (bg : translate x y scaledSprite : enemiesPics)

updateWorld :: Float -> World -> World
updateWorld _ world =
  let (x, y, angle, direction) = getHomelanderPosition world
      keys = getPressedKeys world
      x'
        | Char 'a' `elem` keys = max (x - 10) (-750)
        | Char 'd' `elem` keys = min (x + 10) 750
        | otherwise = x
      y'
        | Char 'w' `elem` keys = min (y + 10) 410
        | Char 's' `elem` keys = max (y - 10) (-410)
        | otherwise = y
      direction'
        | Char 'a' `elem` keys && direction == -1 = 1
        | Char 'd' `elem` keys && direction == 1 = -1
        | otherwise = direction
  in world { mainCharacter = (mainCharacter world) { position = (x', y'), rotation = angle, direction = direction' } }
