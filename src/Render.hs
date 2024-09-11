module Render where

import Graphics.Gloss.Interface.Pure.Game
import Models
import Sprites (GameSprites)
import GHC.IO (unsafePerformIO)

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

