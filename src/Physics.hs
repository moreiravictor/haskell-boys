module Physics where

import Models
import Graphics.Gloss.Interface.Pure.Game

handleInput :: Event -> World -> World
handleInput (EventKey (Char key) Down _ _) world = addPressedKey (Char key) world
handleInput (EventKey (Char key) Up _ _) world = removePressedKey (Char key) world
handleInput (EventMotion mousePos) world = updateRotationWithMouse mousePos world
handleInput (EventKey (MouseButton LeftButton) Down _ _) world = addPressedKey  (MouseButton LeftButton) world
handleInput (EventKey (MouseButton LeftButton) Up _ _) world = removePressedKey  (MouseButton LeftButton) world
handleInput _ world = world

updateRotationWithMouse :: (Float, Float) -> World -> World
updateRotationWithMouse (mouseX, mouseY) world =
  let (charX, charY, _, direction) = getHomelanderPosition world
      deltaX = mouseX - charX
      deltaY = mouseY - charY
      angle = atan2 deltaY deltaX * 180 / pi  -- Convert from radians to degrees
      normalizedAngle = if direction == -1 then angle-20 else - (angle + 180)
  in world { mainCharacter = (mainCharacter world) { rotation = normalizedAngle } }