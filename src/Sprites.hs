module Sprites where

import Graphics.Gloss.Juicy (loadJuicyPNG)
import Graphics.Gloss

data GameSprites = GameSprites
  {
    background :: Picture,
    homelander :: Picture,
    aTrain :: Picture,
    blackNoir :: Picture,
    maeve :: Picture,
    starlight :: Picture,
    theDeep :: Picture,
    translucent :: Picture
  }

getAllSprites :: GameSprites -> [Picture]
getAllSprites sprites =
  [ aTrain sprites
  , blackNoir sprites
  , maeve sprites
  , starlight sprites
  , theDeep sprites
  , translucent sprites
  ]

loadImg :: Maybe Picture -> Picture
loadImg (Just pic) = pic
loadImg Nothing = blank

loadSprites :: IO GameSprites
loadSprites = do
  maybeBg <- loadJuicyPNG "assets/bg.png"
  maybeMainCharacter <- loadJuicyPNG "assets/homelander.png"
  maybeAtrain <- loadJuicyPNG "assets/aTrain.png"
  maybeBlackNoir <- loadJuicyPNG "assets/blackNoir.png"
  maybeMaeve <- loadJuicyPNG "assets/maeve.png"
  maybeStarlight <- loadJuicyPNG "assets/starlight.png"
  maybeTheDeep <- loadJuicyPNG "assets/theDeep.png"
  maybeTranslucent <- loadJuicyPNG "assets/translucent.png"

  let mainCharacterImg = scale 3 3 (loadImg maybeMainCharacter)
  let aTrainImg = scale 3 3 (loadImg maybeAtrain)
  let blackNoirImg = scale 3 3 (loadImg maybeBlackNoir)
  let maeveImg = scale 3 3 (loadImg maybeMaeve)
  let starlightImg = scale 3 3 (loadImg maybeStarlight)
  let theDeepImg = scale 3 3 (loadImg maybeTheDeep)
  let translucentImg = scale 3 3 (loadImg maybeTranslucent)
  let bg = loadImg maybeBg

  return GameSprites
    {
      background = bg,
      homelander = mainCharacterImg,
      aTrain = aTrainImg,
      blackNoir = blackNoirImg,
      maeve = maeveImg,
      starlight = starlightImg,
      theDeep = theDeepImg,
      translucent = translucentImg
    }