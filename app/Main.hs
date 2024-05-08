{-# LANGUAGE OverloadedStrings #-}
module Main where
import Debug.Trace (trace)
import Control.Monad (forM_,unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Events.Keyboard (Keyboard)
import qualified Events.Keyboard as K

import Events.Mouse (MouseState)
import qualified Events.Mouse as MOS

import Config.Config
import Maps.Monde
import Maps.Zones
import Maps.Formes
import qualified Data.Map as Map

import qualified SDL.Video.Renderer as R

import qualified Debug.Trace as T

import Model (GameState)
import qualified Model as M


loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 window_largeur window_hauteur)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')

loadMonde :: Renderer -> FilePath -> FilePath -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadMonde rdr pathSoil pathGrass pathWater tmap smap = do
  tmapSoil <- TM.loadTexture rdr pathSoil (TextureId "soil") tmap
  tmapGrass <- TM.loadTexture rdr pathGrass (TextureId "grass") tmapSoil
  tmapWater <- TM.loadTexture rdr pathWater (TextureId "water") tmapGrass
  -- Create sprites for each terrain type
  let spriteSoil = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "soil") (S.mkArea 0 0 caseSize caseSize)
  let spriteGrass = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "grass") (S.mkArea 0 0 caseSize caseSize)
  let spriteWater = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "water") (S.mkArea 0 0 caseSize caseSize)
  -- Update SpriteMap with new sprites
  let smapUpdated = SM.addSprite (SpriteId "soil") spriteSoil $
                    SM.addSprite (SpriteId "grass") spriteGrass $
                    SM.addSprite (SpriteId "water") spriteWater smap
  return (tmapWater, smapUpdated)

loadToolbox :: Renderer-> FilePath -> FilePath -> FilePath -> FilePath -> FilePath ->TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadToolbox rdr pathTool pathZR pathZI pathZC pathAD tmap smap = do
  tmapTool <- TM.loadTexture rdr pathTool (TextureId "toolbox") tmap
  tmapZR <- TM.loadTexture rdr pathZR (TextureId "ZR_Icon") tmapTool
  tmapZI <- TM.loadTexture rdr pathZI (TextureId "ZI_Icon") tmapZR
  tmapZC <- TM.loadTexture rdr pathZC (TextureId "ZC_Icon") tmapZI
  tmapAD <- TM.loadTexture rdr pathAD (TextureId "AD_Icon") tmapZC
  let spriteToolbox = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "toolbox") (S.mkArea 0 0 largeur_Toolbox hauteur_Toolbox)
  let spriteZR = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "ZR_Icon") (S.mkArea 0 0 largeur_ZRBouton hauteur_ZRBouton)
  let spriteZI = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "ZI_Icon") (S.mkArea 0 0 largeur_ZIBouton hauteur_ZIBouton)
  let spriteZC = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "ZC_Icon") (S.mkArea 0 0 largeur_ZCBouton hauteur_ZCBouton)
  let spriteAD = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "AD_Icon") (S.mkArea 0 0 largeur_ADBouton hauteur_ADBouton)
  let smap' = SM.addSprite (SpriteId "AD_Icon") spriteAD $
              SM.addSprite (SpriteId "ZI_Icon") spriteZI $
              SM.addSprite (SpriteId "ZC_Icon") spriteZC $
              SM.addSprite (SpriteId "ZR_Icon") spriteZR $
              SM.addSprite (SpriteId "toolbox") spriteToolbox smap
  return (tmapAD, smap')

loadBuilding :: Renderer -> FilePath -> FilePath -> FilePath -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBuilding rdr pathZR pathZI pathZC pathAD tmap smap = do
  tmapZR <- TM.loadTexture rdr pathZR (TextureId "ZR_building") tmap
  tmapZI <- TM.loadTexture rdr pathZI (TextureId "ZI_building") tmapZR
  tmapZC <- TM.loadTexture rdr pathZC (TextureId "ZC_building") tmapZI
  tmapAD <- TM.loadTexture rdr pathAD (TextureId "AD_building") tmapZC
  let spriteZR = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "ZR_building") (S.mkArea 0 0 (2*largeur_ZR) (2*hauteur_ZR))
  let spriteZI = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "ZI_building") (S.mkArea 0 0 (2*largeur_ZI) (2*hauteur_ZI))
  let spriteZC = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "ZC_building") (S.mkArea 0 0 (2*largeur_ZC) (2*hauteur_ZC))
  let spriteAD = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "AD_building") (S.mkArea 0 0 (2*largeur_Admin) (2*hauteur_Admin))
  let smap' = SM.addSprite (SpriteId "AD_building") spriteAD $
              SM.addSprite (SpriteId "ZC_building") spriteZC $
              SM.addSprite (SpriteId "ZI_building") spriteZI $
              SM.addSprite (SpriteId "ZR_building") spriteZR smap
  return (tmapAD, smap')

displayMonde :: Renderer -> TextureMap -> SpriteMap -> Monde -> IO ()
displayMonde renderer tmap smap monde = do
    forM_ (Map.toList monde) $ \((C x y), maybeZone) -> do
        zone <- case maybeZone of
            Just z -> return z
            Nothing -> error "displayMonde - No such zone"
        let (C zone_x zone_y) = zoneCoord zone
        let spriteId = case maybeZone of
                Just (Terre _) ->  SpriteId "soil"
                Just (Eau _)   ->  SpriteId "water"
                Just (Grass _) ->  SpriteId "grass"
                Just (ZR _ _)  ->  SpriteId "ZR_building"
                Just (ZI _ _)  ->  SpriteId "ZI_building"
                Just (ZC _ _)  ->  SpriteId "ZC_building"
                Just (Admin _ _) -> SpriteId "AD_building"
        case SM.fetchSprite spriteId smap of
--             sprite -> S.displaySprite renderer tmap (S.moveTo sprite (fromIntegral (x * caseSize)) (fromIntegral (y * caseSize)))
              sprite -> S.displaySprite renderer tmap (S.moveTo sprite (fromIntegral (zone_x * caseSize)) (fromIntegral (zone_y * caseSize)))
--                sprite -> S.displaySprite renderer tmap (S.moveTo sprite (fromIntegral zone_x) (fromIntegral zone_y))


displayToolbox :: Renderer -> TextureMap -> SpriteMap -> IO ()
displayToolbox renderer tmap smap = do
    let zRTexture = SM.fetchSprite (SpriteId "ZR_Icon") smap
    let zITexture = SM.fetchSprite (SpriteId "ZI_Icon") smap
    let zCTexture = SM.fetchSprite (SpriteId "ZC_Icon") smap
    let aDTexture = SM.fetchSprite (SpriteId "AD_Icon") smap
    let toolboxTexture = SM.fetchSprite (SpriteId "toolbox") smap
    S.displaySprite renderer tmap (S.moveTo toolboxTexture (fromIntegral position_Toolbox_x) (fromIntegral position_Toolbox_y))
    S.displaySprite renderer tmap (S.moveTo zRTexture (fromIntegral position_ZRBouton_x) (fromIntegral position_ZRBouton_y))
    S.displaySprite renderer tmap (S.moveTo zITexture (fromIntegral position_ZIBouton_x) (fromIntegral position_ZIBouton_y))
    S.displaySprite renderer tmap (S.moveTo zCTexture (fromIntegral position_ZCBouton_x) (fromIntegral position_ZCBouton_y))
    S.displaySprite renderer tmap (S.moveTo aDTexture (fromIntegral position_ADBouton_x) (fromIntegral position_ADBouton_y))



main :: IO ()
main = do
  -- initialisation de SDL
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 window_largeur window_hauteur }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
--  (tmap, smap) <- loadBackground renderer "assets/grass.bmp" TM.createTextureMap SM.createSpriteMap
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.bmp" tmap smap
  -- chargement du monde
  (tmap'', smap'') <- loadMonde renderer "assets/soil.bmp" "assets/grass.bmp" "assets/water.bmp" tmap' smap'
  -- chargement de la toolbox
  (tmap3, smap3) <- loadToolbox renderer "assets/tool.bmp" "assets/zone_residence.bmp" "assets/zone_industrielle.bmp" "assets/zone_commerciale.bmp" "assets/administratif_bouton.bmp" tmap'' smap''
  -- chargement du batiment
  (tmap4, smap4) <- loadBuilding renderer "assets/ZR_building.bmp" "assets/ZI_building.bmp" "assets/ZC_building.bmp" "assets/administratif_building.bmp" tmap3 smap3
  -- initialisation du monde
  monde <- initMonde window_largeur window_hauteur
  -- initialisation de l'état du jeu
  let gameState = M.initGameState monde
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard

  let mos = MOS.createMouseState
  -- let font = Font.load "assets/DejaVuSans-Bold.ttf" 24
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap4 smap4 kbd mos gameState

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> MouseState -> GameState -> IO ()
gameLoop frameRate renderer tmap smap kbd mos gameState = do
  -- putStrLn "gameLoop"
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  let mos' = MOS.handleEvents events mos
  -- case mos' of
  --   Just (P (V2 x y)) -> Debug.Trace.trace ("Mouse click at:"++ show x ++ " " ++ show y) (return ())
  --   Nothing -> return ()
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  --- display monde
  displayMonde renderer tmap smap (M.monde gameState)
  --- display toolbox
  displayToolbox renderer tmap smap
  --- display perso 
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                 (fromIntegral (M.persoX gameState))
                                 (fromIntegral (M.persoY gameState)))
  ---
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state
  let gameState' = M.gameStep gameState{ M.mouse_state = mos' } kbd' mos' deltaTime
  -- let gameState'' = gameState' { M.mouseClick = Nothing }

  case M.displayText gameState' of
    Just text -> putStrLn text
    Nothing -> return ()
  -- clear displayText
  let gameState'' = gameState' { M.displayText = Nothing }

  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' mos' gameState'')
