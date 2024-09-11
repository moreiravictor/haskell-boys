module Sprites where

import Graphics.Gloss.Juicy (loadJuicyPNG, loadJuicyJPG)
import Graphics.Gloss

data GameSprites = GameSprites
  {
    stage1Background  :: Picture,
    stage2Background  :: Picture,
    stage3Background  :: Picture,
    menuBackground    :: Picture,
    loseBackground    :: Picture,
    winBackground    :: Picture,
    homelander        :: Picture,
    aTrain            :: Picture,
    blackNoir         :: Picture,
    maeve             :: Picture,
    starlight         :: Picture,
    theDeep           :: Picture,
    translucent       :: Picture,
    laser             :: Picture,
    logo              :: Picture,
    start             :: Picture,
    heart             :: Picture,
    loseTitle         :: Picture,
    loseSubtitle      :: Picture,
    winTitle          :: Picture
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
  maybeLoseBg <- loadJuicyPNG "assets/backgrounds/lostBg.png"
  maybeMenuBg <- loadJuicyJPG "assets/backgrounds/menuBg.jpg"
  maybeStage1Bg <- loadJuicyPNG "assets/backgrounds/stage1Bg.png"
  maybeStage2Bg <- loadJuicyJPG "assets/backgrounds/stage2Bg.jpg"
  maybeStage3Bg <- loadJuicyJPG "assets/backgrounds/stage3Bg.jpg"
  maybeWinBg <- loadJuicyJPG "assets/backgrounds/winBg.jpg"

  maybeMainCharacter <- loadJuicyPNG "assets/characters/homelander.png"
  maybeAtrain <- loadJuicyPNG "assets/characters/aTrain.png"
  maybeBlackNoir <- loadJuicyPNG "assets/characters/blackNoir.png"
  maybeMaeve <- loadJuicyPNG "assets/characters/maeve.png"
  maybeStarlight <- loadJuicyPNG "assets/characters/starlight.png"
  maybeTheDeep <- loadJuicyPNG "assets/characters/theDeep.png"
  maybeTranslucent <- loadJuicyPNG "assets/characters/translucent.png"

  maybeLaser <- loadJuicyPNG "assets/items/laser.png"
  maybeHeart <- loadJuicyPNG "assets/items/heart.png"

  maybeLogo <- loadJuicyPNG "assets/texts/logo.png"
  maybeStart <- loadJuicyPNG "assets/texts/start.png"
  maybeLoseTitle <- loadJuicyPNG "assets/texts/lose.png"
  maybeLoseSubtitle <- loadJuicyPNG "assets/texts/loseCommands.png"
  maybeWinTitle <- loadJuicyPNG "assets/texts/win.png"

  let mainCharacterImg = scale 3 3 $ loadImg maybeMainCharacter
  let aTrainImg = scale 3 3 $ loadImg maybeAtrain
  let blackNoirImg = scale 3 3 $ loadImg maybeBlackNoir
  let maeveImg = scale 3 3 $ loadImg maybeMaeve
  let starlightImg = scale 3 3 $ loadImg maybeStarlight
  let theDeepImg = scale 3 3 $ loadImg maybeTheDeep
  let translucentImg = scale 3 3 $ loadImg maybeTranslucent


  let stage1Bg = loadImg maybeStage1Bg
  let stage2Bg = loadImg maybeStage2Bg
  let stage3Bg = loadImg maybeStage3Bg
  let menuBgImg = scale 2.5 2.5 $ loadImg maybeMenuBg
  let loseBgImg = scale 1.5 1.5 $ loadImg maybeLoseBg
  let winBgImg = scale 1 1 $ loadImg maybeWinBg

  let laserImg = scale 1.5 1.5 $ loadImg maybeLaser
  let heartImg = scale 0.3 0.3 $ loadImg maybeHeart

  let logoImg = loadImg maybeLogo
  let startImg = loadImg maybeStart
  let loseTitleImg = scale 1 1 $ loadImg maybeLoseTitle
  let loseSubtitleImg = scale 1 1 $ loadImg maybeLoseSubtitle
  let winTitleImg = scale 1 1 $ loadImg maybeWinTitle

  return GameSprites
    {
      stage1Background = stage1Bg,
      stage2Background = stage2Bg,
      stage3Background = stage3Bg,
      menuBackground = menuBgImg,
      loseBackground = loseBgImg,
      winBackground = winBgImg,
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
      loseSubtitle = loseSubtitleImg,
      winTitle = winTitleImg
    }
