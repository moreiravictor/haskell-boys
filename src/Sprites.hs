module Sprites where

import Graphics.Gloss.Juicy (loadJuicyPNG)
import Graphics.Gloss

data GameSprites = GameSprites
  {
    homelander :: Picture,
    background :: Picture
  }

loadImg :: Maybe Picture -> Picture
loadImg (Just pic) = pic
loadImg Nothing = blank

loadSprites :: IO GameSprites
loadSprites = do
  maybeMainCharacter <- loadJuicyPNG "assets/homelander.png"
  maybeBg <- loadJuicyPNG "assets/bg.png"

  let mainCharacterImg = loadImg maybeMainCharacter
      bg = loadImg maybeBg

      rescaled = scale 0.5 0.5 mainCharacterImg

  return GameSprites { homelander = rescaled, background = bg }