module Render where

import Graphics.Gloss.Interface.Pure.Game
import Models
import GHC.IO (unsafePerformIO)
import Control.Monad.State
import Control.Monad (when)
import Sprites
import Data.Maybe (maybeToList)
import Physics (updateLasers, handleCollisions)
import System.Random (randomRIO)

-- Below are functions used to draw scenarios
drawLasers :: World -> [Picture]
drawLasers world = map (\laser -> uncurry translate (pPosition laser) (pSprite laser)) (projectiles world)

drawEnemies :: World -> [Picture]
drawEnemies world = map (\enemy -> uncurry translate (ePosition enemy) (eSprite enemy)) (enemies world)

drawHomelander :: World -> Picture
drawHomelander world = finalPic
  where (x, y) = position (mainCharacter world)
        charSprite = sprite (mainCharacter world)
        orientation = direction (mainCharacter world)
        rotatedSprite = rotate (rotation (mainCharacter world)) charSprite
        scaledSprite = scale orientation 1 rotatedSprite
        parsedHomelanderPic = translate x y scaledSprite
        finalPic
          | (snd (blink $ mainCharacter world) /= 0) && even (floor (snd (blink $ mainCharacter world) / 4)) = Blank
          | otherwise = parsedHomelanderPic

-- Function used to generate waves of enemies on the screen
updateEnemies :: [Enemy] -> GameSprites -> Int -> [Enemy]
updateEnemies enemies' sprites enemiesAmount' = if not (null enemies') then (removeMissedEnemies . map walk) enemies' else unsafePerformIO (generateEnemies sprites enemiesAmount')
  where
    walk enemy =
      let (initialX, initialY) = ePosition enemy
          (x, y) = case eInitialBorder enemy of
            1 -> (initialX, initialY - 4) -- Top border
            2 -> (initialX, initialY + 4) -- Bottom border
            3 -> (initialX + 4, initialY) -- Left border
            _ -> (initialX - 4, initialY) -- Right border
      in enemy {ePosition = (x, y)}
    removeMissedEnemies :: [Enemy] -> [Enemy]
    removeMissedEnemies = filter isVisible
      where
        isVisible enemy =
          let (x', y') = ePosition enemy
          in x' >= -900 && x' <= 900 && y' >= -600 && y' <= 600

drawBoldText :: String -> Color -> Picture
drawBoldText text' color' =
  pictures [translate offsetX offsetY (color color' (text text')) | offsetX <- [-3, 0, 3], offsetY <- [-3, 0, 3]]

-- Below are functions used to draw elements on the screen on correct sizes, positions and with some animations
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

-- Below are functions used to update elements on playing world
moveElements :: GameMonad ()
moveElements = do
      (x, y, angle, direction') <- gets getHomelanderPosition
      keys <- gets pressedKeys
      laserSprite <- gets (laser . gameSprites)
      currentProjectiles <- gets projectiles
      let (screenWidth, screenHeight) = screenSize
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
          projectile'
            | MouseButton LeftButton `elem` keys = Just Projectile { pSprite =  laserSprite, pPosition = (x', y'), pDirection = direction'', pRotation = angle  }
            | otherwise = Nothing
      modify $ \world -> world {
        mainCharacter = (mainCharacter world) { position = (x', y'), rotation = angle, direction = direction'' },
        projectiles = updateLasers currentProjectiles ++ maybeToList projectile'
      }

updateCollisions :: GameMonad ()
updateCollisions = do
      currentProjectiles <- gets projectiles
      enemies' <- gets enemies
      stats' <- gets stats
      mainCharacter' <- gets mainCharacter
      (_, blinkTimer) <- gets (blink . mainCharacter)
      let (life', updatedEnemies) = handleCollisions (life stats') mainCharacter'  currentProjectiles enemies'
          kills' =  kills stats' + (length enemies' - length updatedEnemies)
          startBlink = life' < life stats'
          updatedTimer = if startBlink then 60 else max 0 (blinkTimer - 0.5)
          newIsBlinking = updatedTimer /= 0
      modify $ \world -> world {
        stats = GameStats { kills = kills', life = life' },
        mainCharacter = mainCharacter' { blink = (newIsBlinking, updatedTimer) },
        enemies = updateEnemies updatedEnemies (gameSprites world) (enemiesAmount world)
      }

updateStage :: GameMonad ()
updateStage = do
  currentGameState <- gets gameState
  stats' <- gets stats
  if life stats' <= 0
  then modify $ \world -> world { stats = initialStats, enemiesAmount = 0, gameState = GameOver, enemies = [] }
  else
    case currentGameState of
      Stage1 -> do
        currentKills <- gets (kills . stats)
        when (currentKills >= 10) $ do
          modify $ \world -> world { stats = initialStats, enemiesAmount = stage2EnemiesAmount, gameState = Stage2, enemies = [] }
      Stage2 -> do
        currentKills <- gets (kills . stats)
        when (currentKills >= 15) $ do
          modify $ \world -> world { stats = initialStats, enemiesAmount = stage3EnemiesAmount, gameState = Stage3, enemies = [] }
      Stage3 -> do
        currentKills <- gets (kills . stats)
        when (currentKills >= 20) $ do
          modify $ \world -> world { stats = initialStats, enemiesAmount = 0, gameState = Win, enemies = [] }
      _ -> return ()

updateWorld :: Float -> GameMonad ()
updateWorld dt = do
      currentGameState <- gets gameState
      case currentGameState of
          Stage1   -> updatePlayingWorld
          Stage2   -> updatePlayingWorld
          Stage3   -> updatePlayingWorld
          Menu     -> updateStaticWorld dt
          Win      -> updateStaticWorld dt
          GameOver -> updateStaticWorld dt

updateStaticWorld :: Float -> GameMonad ()
updateStaticWorld dt = do
  modify (\world -> world { time = time world + dt })

updatePlayingWorld :: GameMonad ()
updatePlayingWorld = do
      moveElements
      updateCollisions
      updateStage

-- Function used to generate enemies randomly on border of game screen so that they do not just appear on screen but walk into it
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
          return (xPos, halfHeight + 100)  -- above the top
        2 -> do -- Bottom border
          xPos <- randomRIO (-halfWidth, halfWidth)
          return (xPos, -(halfHeight + 100))  -- below the bottom
        3 -> do -- Left border
          yPos <- randomRIO (-halfHeight, halfHeight)
          return (-(halfWidth + 100), yPos)  -- outside the left
        _ -> do -- Right border
          yPos <- randomRIO (-halfHeight, halfHeight)
          return (halfWidth + 100, yPos)  -- outside the right

      return $ Enemy sprite' (x, y) 1 borderSide
