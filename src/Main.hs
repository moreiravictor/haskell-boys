module Main (main) where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicyPNG)

data World = World
  {  background :: Picture,
     sprite :: Picture,
     position :: (Float, Float)
  }

loadImg :: Maybe Picture -> Picture
loadImg (Just pic) = pic
loadImg Nothing = blank

main :: IO ()
main = do
  maybeMainCharacter <- loadJuicyPNG "assets/homelander.png"
  maybeBg <- loadJuicyPNG "assets/bg.png"

  let mainCharacter = loadImg maybeMainCharacter
  let bg = loadImg maybeBg

  -- Inicializa o mundo com o sprite carregado
  let initialWorld = World bg mainCharacter

  -- Inicia o jogo
  play
    (InWindow "Haskell Boys" (1280, 720) (0, 0)) -- Janela do jogo
    white                                         -- Cor de fundo
    60                                           -- FPS
    initialWorld                                 -- Estado inicial do mundo
    drawWorld                                    -- Função de desenho
    handleInput                                  -- Função de processamento de eventos
    updateWorld                                  -- Função de atualização do mundo

-- Função de desenho do mundo
drawWorld :: World -> Picture
drawWorld world = pictures [background world, translate 0 0 (sprite world)]

-- Função para lidar com eventos de entrada (neste caso, não faz nada)
handleInput :: Event -> World -> World
handleInput _ world = world

-- Função de atualização do mundo (neste caso, não faz nada)
updateWorld :: Float -> World -> World
updateWorld _ world = world
