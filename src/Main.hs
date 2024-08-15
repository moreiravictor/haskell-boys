module Main (main) where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Models
import Physics
import Sprites

main :: IO ()
main = do

  sprites <- loadSprites

  let mainCharacter = Homelander (homelander sprites) (0,0)

  let initialWorld = World (background sprites) mainCharacter []

  play
    (InWindow "Haskell Boys" (1280, 720) (0, 0)) -- Janela do jogo
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
  in pictures [bg, translate x y charSprite]

updateWorld :: Float -> World -> World
updateWorld _ world =
  let (x, y) = getHomelanderPosition world
      keys = getPressedKeys world
      x'
        | SpecialKey KeyLeft `elem` keys = x - 10
        | SpecialKey KeyRight `elem` keys = x + 10
        | otherwise = x
      y'
        | SpecialKey KeyUp `elem` keys = y + 10
        | SpecialKey KeyDown `elem` keys = y - 10
        | otherwise = y
  in setHomelanderPosition (x', y') world
