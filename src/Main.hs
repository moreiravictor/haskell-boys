module Main (main) where

import Graphics.Gloss
import Models
import Physics
import Sprites
import Render
import Data.Bifunctor (Bifunctor(bimap))
import Control.Monad.State

main :: IO ()
main = do

  sprites <- loadSprites

  let mainChar = Homelander (homelander sprites) (0,0) 0 (-1) (False, 0)

  let initialWorld = World Menu initialStats 0 sprites mainChar [] [] stage1EnemiesAmount []

  play
    (InWindow "Haskell Boys" (bimap round round screenSize) screenPosition) -- Janela do jogo
    white                                         -- Cor de fundo
    60                                           -- FPS
    initialWorld
    drawWorld
    handleInput
    (execState . updateWorld)
