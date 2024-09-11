module Physics where

import Models
import Graphics.Gloss.Interface.Pure.Game
import Sprites (GameSprites(homelander))

handleInput :: Event -> World -> World
handleInput (EventKey (Char key) Down _ _) world = addPressedKey (Char key) world
handleInput (EventKey (Char key) Up _ _) world = removePressedKey (Char key) world
handleInput (EventMotion mousePos) world = updateRotationWithMouse mousePos world
handleInput (EventKey (MouseButton LeftButton) Down _ _) world = addPressedKey  (MouseButton LeftButton) world
handleInput (EventKey (MouseButton LeftButton) Up _ _) world = removePressedKey  (MouseButton LeftButton) world
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) world =
  case gameState world of
    Menu -> world { gameState = Stage1 }
    GameOver ->
      let mainChar = Homelander (homelander $ gameSprites world)  (0,0) 0 (-1) (False, 0)
      in World Stage1 initialStats 0 (gameSprites world) mainChar [] [] 10 []
    _ -> world
handleInput _ world = world

updateRotationWithMouse :: (Float, Float) -> World -> World
updateRotationWithMouse (mouseX, mouseY) world =
  let (charX, charY, _, direction') = getHomelanderPosition world
      deltaX = mouseX - charX
      deltaY = mouseY - charY
      angle = atan2 deltaY deltaX * 180 / pi  -- Convert from radians to degrees
      normalizedAngle = if direction' == -1 then angle-20 else - (angle + 180)
  in world { mainCharacter = (mainCharacter world) { rotation = normalizedAngle } }

updateLasers :: [Projectile] -> [Projectile]
updateLasers = removeMissedLasers . map walk
  where
    walk projectile' =
        let
            speed = 10
            normalizedAngle = if pDirection projectile' == -1 then pRotation projectile' + 20 else - pRotation projectile' - 360
            radianAngle = normalizedAngle * pi / 180  -- Convert rotation angle to radians
            velX = speed * cos radianAngle * (- pDirection projectile')
            velY = speed * sin radianAngle * (- pDirection projectile')
            (x, y) = pPosition projectile'
        in projectile' {pPosition = (x + velX, y + velY)}
    removeMissedLasers :: [Projectile] -> [Projectile]
    removeMissedLasers = filter isVisible
      where
        isVisible projectile' =
          let (x, y) = pPosition projectile'
          in x >= -850 && x <= 850 && y >= -470 && y <= 470

handleCollisions :: Int -> Homelander -> [Projectile] -> [Enemy] -> (Int, [Enemy])
handleCollisions life' homelander' projectiles' = foldr checkCollision (life', [])
  where
    laserCollides projectile enemy =
      let (px, py) = pPosition projectile
          (ex, ey) = ePosition enemy
          size = 40
      in abs (px - ex) < size && abs (py - ey) < size
    homelanderCollides homelander'' enemy =
      let (hx, hy) = position homelander''
          (ex, ey) = ePosition enemy
          size = 80
      in abs (hx - ex) < size && abs (hy - ey) < size
    checkCollision enemy (life'', remainingEnemies)
      | any (`laserCollides` enemy) projectiles' = (life'', remainingEnemies)
      | homelanderCollides homelander' enemy = (life'' - 1, remainingEnemies)
      | otherwise = (life'', enemy : remainingEnemies)
