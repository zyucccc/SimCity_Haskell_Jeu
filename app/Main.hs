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

displayMonde :: Renderer -> TextureMap -> SpriteMap -> Monde -> IO ()
displayMonde renderer tmap smap monde = do
    forM_ (Map.toList monde) $ \((C x y), maybeZone) -> do
        let spriteId = case maybeZone of
                Just (Terre _) -> SpriteId "soil"
                Just (Eau _)   -> SpriteId "water"
                Just (Grass _) -> SpriteId "grass"
        case SM.fetchSprite spriteId smap of
             sprite -> S.displaySprite renderer tmap (S.moveTo sprite (fromIntegral (x * caseSize)) (fromIntegral (y * caseSize)))
             _ -> return ()  -- 如果没有找到精灵，不做任何操作


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
  -- initialisation du monde
  monde <- initMonde window_largeur window_hauteur
  -- initialisation de l'état du jeu
  let gameState = M.initGameState monde
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard

  let mos = MOS.createMouseState
  -- let font = Font.load "assets/DejaVuSans-Bold.ttf" 24
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap'' smap'' kbd mos gameState

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
