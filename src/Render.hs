module Render where

import Graphics.Gloss.Interface.Pure.Game
import Models

drawLasers :: World -> [Picture]
drawLasers world = map (\laser -> uncurry translate (pPosition laser) (pSprite laser)) (projectiles world)

drawEnemies :: World -> [Picture]
drawEnemies world = map (\enemy -> uncurry translate (ePosition enemy) (eSprite enemy)) (enemies world)

drawHomelander :: World -> Picture
drawHomelander world = parsedHomelanderPic
  where (x, y) = position (mainCharacter world)
        charSprite = sprite (mainCharacter world)
        orientation = direction (mainCharacter world)
        rotatedSprite = rotate (rotation (mainCharacter world)) charSprite
        scaledSprite = scale orientation 1 rotatedSprite
        parsedHomelanderPic = translate x y scaledSprite