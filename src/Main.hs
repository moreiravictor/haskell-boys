module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Models
import Physics
import Sprites
import Render
import Data.Maybe (isJust, fromJust)
import Data.Bifunctor (Bifunctor(bimap))

main :: IO ()
main = do

  sprites <- loadSprites

  let mainChar = Homelander (homelander sprites) (0,0) 0 (-1)

  let initialWorld = World Menu 0 sprites mainChar [] [] []

  play
    (InWindow "Haskell Boys" (bimap round round screenSize) screenPosition) -- Janela do jogo
    white                                         -- Cor de fundo
    60                                           -- FPS
    initialWorld
    drawWorld
    handleInput
    updateWorld

drawWorld :: World -> Picture
drawWorld world =
  case gameState world of
    Menu      -> drawMenu world
    Playing   -> drawPlayingWorld world
    GameOver  -> drawGameOver

drawMenu :: World -> Picture
drawMenu world =
  let
      logoVerticalOffset = 5 * sin (4 * time world)
      logo' = translate 0 (100 + logoVerticalOffset) $ logo (gameSprites world)
      scaleFactor = 0.6 + 0.2 * sin (2.5 * time world)
      start' = translate (-20) (-220) $ scale scaleFactor scaleFactor $ start (gameSprites world)
      bg = menuBackground (gameSprites world)
  in pictures [bg, logo', start']

drawGameOver :: Picture
drawGameOver = translate (-300) 0 $ scale 0.3 0.3 $ text "GAME OVER!"

drawPlayingWorld :: World -> Picture
drawPlayingWorld world =
  let backgroundPic = background (gameSprites world)
      homelanderPic = drawHomelander world
      enemiesPics = drawEnemies world
      laserPics = drawLasers world
  in pictures (backgroundPic : homelanderPic : enemiesPics ++ laserPics)

updateWorld :: Float -> World -> World
updateWorld dt world = case gameState world of
            Playing -> updatePlayingWorld world
            Menu -> updateMenuWorld dt world
            GameOver -> updatePlayingWorld world

updateMenuWorld :: Float -> World -> World
updateMenuWorld dt world = world { time = time world + dt }

updatePlayingWorld :: World -> World
updatePlayingWorld world =
  let (x, y, angle, direction) = getHomelanderPosition world
      keys = getPressedKeys world
      currentProjectiles = projectiles world
      enemies' = getEnemies world
      (screenWidth, screenHeight) = screenSize
      x'
        | Char 'a' `elem` keys || Char 'A' `elem` keys = max (x - 10) (- (screenWidth/2))
        | Char 'd' `elem` keys || Char 'D' `elem` keys = min (x + 10) (screenWidth/2)
        | otherwise = x
      y'
        | Char 'w' `elem` keys || Char 'W' `elem` keys = min (y + 10) (screenHeight/2)
        | Char 's' `elem` keys || Char 'S' `elem` keys = max (y - 10) (- (screenHeight/2))
        | otherwise = y
      direction'
        | (Char 'a' `elem` keys || Char 'A' `elem` keys) && direction == -1 = 1
        | (Char 'd' `elem` keys || Char 'D' `elem` keys) && direction == 1 = -1
        | otherwise = direction
      projectile' :: Maybe Projectile
      projectile'
        | MouseButton LeftButton `elem` keys = Just Projectile { pSprite = laser (gameSprites world), pPosition = (x', y'), pDirection = direction', pRotation = angle  }
        | otherwise = Nothing
      updatedEnemies = removeCollidedEnemies currentProjectiles enemies'
  in world {
    mainCharacter = (mainCharacter world) { position = (x', y'), rotation = angle, direction = direction' },
    projectiles = updateLasers currentProjectiles ++ [fromJust projectile' | isJust projectile'],
    enemies = updateEnemies updatedEnemies (gameSprites world)
  }
