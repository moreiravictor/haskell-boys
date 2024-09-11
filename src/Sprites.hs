module Sprites where

import Graphics.Gloss.Juicy (loadJuicyPNG, loadJuicyJPG)
import Graphics.Gloss

data GameSprites = GameSprites
  {
    background      :: Picture,
    menuBackground  :: Picture,
    loseBackground  :: Picture,
    homelander      :: Picture,
    aTrain          :: Picture,
    blackNoir       :: Picture,
    maeve           :: Picture,
    starlight       :: Picture,
    theDeep         :: Picture,
    translucent     :: Picture,
    laser           :: Picture,
    logo            :: Picture,
    start           :: Picture,
    heart           :: Picture,
    loseTitle       :: Picture,
    loseSubtitle    :: Picture
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
  maybeLaser <- loadJuicyPNG "assets/laser.png"
  maybeLogo <- loadJuicyPNG "assets/logo.png"
  maybeStart <- loadJuicyPNG "assets/start.png"
  maybeMenuBg <- loadJuicyJPG "assets/menuBg.jpg"
  maybeHeart <- loadJuicyPNG "assets/heart.png"
  maybeLoseTitle <- loadJuicyPNG "assets/lose.png"
  maybeLoseSubtitle <- loadJuicyPNG "assets/loseCommands.png"
  maybeLoseBg <- loadJuicyPNG "assets/bgLose.png"

  let mainCharacterImg = scale 3 3 $ loadImg maybeMainCharacter
  let aTrainImg = scale 3 3 $ loadImg maybeAtrain
  let blackNoirImg = scale 3 3 $ loadImg maybeBlackNoir
  let maeveImg = scale 3 3 $ loadImg maybeMaeve
  let starlightImg = scale 3 3 $ loadImg maybeStarlight
  let theDeepImg = scale 3 3 $ loadImg maybeTheDeep
  let translucentImg = scale 3 3 $ loadImg maybeTranslucent
  let laserImg = scale 1.5 1.5 $ loadImg maybeLaser
  let bg = loadImg maybeBg
  let logoImg = loadImg maybeLogo
  let startImg = loadImg maybeStart
  let menuBgImg = scale 2.5 2.5 $ loadImg maybeMenuBg
  let heartImg = scale 0.3 0.3 $ loadImg maybeHeart
  let loseTitleImg = scale 1 1 $ loadImg maybeLoseTitle
  let loseSubtitleImg = scale 1 1 $ loadImg maybeLoseSubtitle
  let loseBgImg = scale 1 1 $ loadImg maybeLoseBg

  return GameSprites
    {
      background = bg,
      menuBackground = menuBgImg,
      loseBackground = loseBgImg,
      homelander = mainCharacterImg,
      aTrain = aTrainImg,
      blackNoir = blackNoirImg,
      maeve = maeveImg,
      starlight = starlightImg,
      theDeep = theDeepImg,
      translucent = translucentImg,
      laser = laserImg,
      logo = logoImg,
      start = startImg,
      heart = heartImg,
      loseTitle = loseTitleImg,
      loseSubtitle = loseSubtitleImg
    }