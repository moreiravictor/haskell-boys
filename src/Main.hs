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

  let mainChar = Homelander (homelander sprites) (0,0) 0 (-1) (False, 0)

  let initialWorld = World Menu initialStats 0 sprites mainChar [] [] 15 []

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
    Stage1    -> drawPlayingWorld world
    Stage2    -> drawPlayingWorld world
    Stage3    -> drawPlayingWorld world
    Win       -> drawWinWorld world
    GameOver  -> drawGameOver world

drawMenu :: World -> Picture
drawMenu world =
  let
      logoVerticalOffset = 5 * sin (4 * time world)
      logo' = translate 0 (100 + logoVerticalOffset) $ logo (gameSprites world)
      scaleFactor = 0.6 + 0.2 * sin (2.5 * time world)
      start' = translate (-20) (-220) $ scale scaleFactor scaleFactor $ start (gameSprites world)
      bg = menuBackground (gameSprites world)
  in pictures [bg, logo', start']

drawWinWorld :: World -> Picture
drawWinWorld world =
  let
    logoVerticalOffset = 5 * sin (4 * time world)
    bg = winBackground $ gameSprites world
    logo' = translate 0 (100 + logoVerticalOffset) $ winTitle $ gameSprites world
  in pictures [bg, logo']

drawGameOver :: World -> Picture
drawGameOver world =
  let
    logoVerticalOffset = 5 * sin (4 * time world)
    bg = loseBackground $ gameSprites world
    logo' = translate 0 (100 + logoVerticalOffset) $ loseTitle $ gameSprites world
    scaleFactor = 0.8 + 0.1 * sin (2.5 * time world)
    lose' = translate (-20) (-220) $ scale scaleFactor scaleFactor $ loseSubtitle (gameSprites world)
  in pictures [bg, logo', lose']

drawPlayingWorld :: World -> Picture
drawPlayingWorld world =
  let backgroundPic = case gameState world of
        Stage1 -> stage1Background (gameSprites world)
        Stage2 -> stage2Background (gameSprites world)
        Stage3 -> stage3Background (gameSprites world)
        _ -> Blank
      homelanderPic = drawHomelander world
      enemiesPics = drawEnemies world
      laserPics = drawLasers world
      heartPic = heart $ gameSprites world
      score = translate (-750) 350 $ scale 0.5 0.5 $ drawBoldText ("Kills: " ++ show (kills $ stats world)) red
      lifes = [translate (540 + fromIntegral i * 90) 380 heartPic | i <- [0 .. life (stats world) - 1]]
  in pictures (backgroundPic : score : homelanderPic : enemiesPics ++ laserPics ++ lifes)

updateWorld :: Float -> World -> World
updateWorld dt world = case gameState world of
            Stage1 -> updatePlayingWorld world
            Stage2 -> updatePlayingWorld world
            Stage3 -> updatePlayingWorld world
            Menu -> updateStaticWorld dt world
            Win -> updateStaticWorld dt world
            GameOver -> updateStaticWorld dt world

updateStaticWorld :: Float -> World -> World
updateStaticWorld dt world = world { time = time world + dt }

updatePlayingWorld :: World -> World
updatePlayingWorld world =
  let (x, y, angle, direction') = getHomelanderPosition world
      keys = getPressedKeys world
      currentProjectiles = projectiles world
      enemies' = getEnemies world
      stats' = stats world
      (_, blinkTimer) = blink $ mainCharacter world
      (screenWidth, screenHeight) = screenSize
      x'
        | Char 'a' `elem` keys || Char 'A' `elem` keys = max (x - 10) (- (screenWidth/2))
        | Char 'd' `elem` keys || Char 'D' `elem` keys = min (x + 10) (screenWidth/2)
        | otherwise = x
      y'
        | Char 'w' `elem` keys || Char 'W' `elem` keys = min (y + 10) (screenHeight/2)
        | Char 's' `elem` keys || Char 'S' `elem` keys = max (y - 10) (- (screenHeight/2))
        | otherwise = y
      direction''
        | (Char 'a' `elem` keys || Char 'A' `elem` keys) && direction' == -1 = 1
        | (Char 'd' `elem` keys || Char 'D' `elem` keys) && direction' == 1 = -1
        | otherwise = direction'
      projectile' :: Maybe Projectile
      projectile'
        | MouseButton LeftButton `elem` keys = Just Projectile { pSprite = laser (gameSprites world), pPosition = (x', y'), pDirection = direction'', pRotation = angle  }
        | otherwise = Nothing
      (life', updatedEnemies) = handleCollisions (life stats') (mainCharacter world)  currentProjectiles enemies'
      kills' =  kills stats' + (length enemies' - length updatedEnemies)
      startBlink = life' < life stats'
      updatedTimer = if startBlink then 60 else max 0 (blinkTimer - 0.5)
      newIsBlinking = updatedTimer /= 0
      updatedInfo = updateStage world
      (updatedStats, newEnemyCount, newGameState, newEnemies) = case updatedInfo of
        Just (newStats, newEnemyNum, newStage, nullEnemies) -> (newStats, newEnemyNum, newStage, nullEnemies)
        Nothing -> (GameStats { kills = kills', life = life' }, 15, gameState world, updatedEnemies)
      gameState' = if life' > 0 then newGameState else GameOver
  in world {
    mainCharacter = (mainCharacter world) { position = (x', y'), rotation = angle, direction = direction'', blink = (newIsBlinking, updatedTimer) },
    projectiles = updateLasers currentProjectiles ++ [fromJust projectile' | isJust projectile'],
    enemies = updateEnemies newEnemies (gameSprites world) newEnemyCount,
    stats = updatedStats,
    gameState = gameState'
  }
-- check for kills amount to update
-- change background
-- change enemies amount
-- reset life and kills
updateStage :: World -> Maybe (GameStats, Int, GameState, [Enemy])
updateStage world = case gameState world of
    Stage1 ->
      if kills (stats world) >= 10
      then Just (initialStats, 20, Stage2, [])
      else Nothing
    Stage2 ->
      if kills (stats world) >= 15
      then Just (initialStats, 25, Stage3, [])
      else Nothing
    Stage3 ->
      if kills (stats world) >= 20
      then Just (initialStats, 0, Win, [])
      else Nothing
    _ -> Nothing