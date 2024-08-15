module Physics where

import Models
import Graphics.Gloss.Interface.Pure.Game

handleInput :: Event -> World -> World
handleInput (EventKey (SpecialKey key) Down _ _) world = addPressedKey (SpecialKey key) world
handleInput (EventKey (SpecialKey key) Up _ _) world = removePressedKey (SpecialKey key) world
handleInput _ world = world