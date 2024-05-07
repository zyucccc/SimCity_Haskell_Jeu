
module Model where

import SDL

import GHC.Int (Int32)

import Events.Keyboard (Keyboard)
import qualified Events.Keyboard as K

import Events.Mouse (MouseState)
import qualified Events.Mouse as MOS

import Maps.Formes
import Maps.Monde
import Maps.Zones
import Entitys.Entitys

import Debug.Trace (trace)
import Config.Config

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int
                           , mouse_state :: MouseState
                           , displayText :: Maybe String
                           , monde :: Monde
                           , selectedZone :: Maybe ZoneType
                           }
                           deriving (Show)


initGameState :: Monde -> GameState
initGameState monde = GameState 200 300 4 (False,Nothing) Nothing monde Nothing

moveLeft :: GameState -> GameState
moveLeft gs = Debug.Trace.trace "Keyboard: Move left" gs { persoX = persoX gs - speed gs }

moveRight :: GameState -> GameState
moveRight gs =Debug.Trace.trace "Keyboard: Move Right"  gs { persoX = persoX gs + speed gs }
                              
moveUp :: GameState -> GameState
moveUp gs = Debug.Trace.trace "Keyboard: Move Up"  gs  { persoY = persoY gs - speed gs }

moveDown :: GameState -> GameState
moveDown gs = Debug.Trace.trace "Keyboard: Move Down"  gs { persoY = persoY gs + speed gs }

gameStep :: RealFrac a => GameState -> Keyboard -> MouseState -> a -> GameState
gameStep gstate kbd mos deltaTime =
  let modif = (if K.keypressed KeycodeLeft kbd
               then moveLeft else id)
              .
              (if K.keypressed KeycodeRight kbd
               then moveRight else id)
              .
              (if K.keypressed KeycodeUp kbd
               then moveUp else id)
              .
              (if K.keypressed KeycodeDown kbd
               then moveDown else id)
              .
              (if MOS.mousePressed mos
               then handleMouseClick else id)
               in modif gstate

handleMouseClick :: GameState -> GameState
handleMouseClick gs = handleMouseClick_BatimentType $ handleMouseClick_BuildZone $ handleMouseClick_touche gs

handleMouseClick_BuildZone :: GameState -> GameState
handleMouseClick_BuildZone gs =
  let (pressed,pos) = mouse_state gs
      px = fromIntegral $ persoX gs
      py = fromIntegral $ persoY gs
      notre_monde = monde gs
  in case pressed of
        False -> gs
        True -> case pos of
                     Just (P (V2 x y)) ->
                            let coord_pixel = C (fromIntegral x) (fromIntegral y)
                                coord_case = coordToRowCol coord_pixel
                                in case selectedZone gs of
                                        Just ZRType -> case check_DejaBuild_Monde ZRType coord_pixel notre_monde of
                                            True -> gs
                                            False -> gs {displayText = Just "build ZRType", monde = placeZone (createZone_ZR coord_pixel) notre_monde }
                                        _ -> gs
                     Nothing -> gs

--handleMouseClick_BuildZone :: GameState -> GameState
--handleMouseClick_BuildZone gs =
--  let (pressed,pos) = mouse_state gs
--      px = fromIntegral $ persoX gs
--      py = fromIntegral $ persoY gs
--      notre_monde = monde gs
--  in case pressed of
--        False -> gs
--        True -> case pos of
--                     Just (P (V2 x y)) ->
--                            let coord_pixel = C (fromIntegral x) (fromIntegral y)
--                                coord_case = coordToRowCol coord_pixel
--                                in case checkCoord_Monde coord_case notre_monde of
--                                    True -> gs
--                                    False -> case selectedZone gs of
--                                        Just ZRType -> gs {displayText = Just "build ZRType", monde = placeZone (createZone_ZR coord_pixel) notre_monde }
--                                        _ -> gs
--                     Nothing -> gs

handleMouseClick_BatimentType :: GameState -> GameState
handleMouseClick_BatimentType gs =
  let (pressed,pos) = mouse_state gs
      px = fromIntegral $ persoX gs
      py = fromIntegral $ persoY gs
  in case pressed of
        False -> gs
        True -> case pos of
                     Just (P (V2 x y)) ->
                            if  x > fromIntegral position_ZRBouton_x  && x < fromIntegral (position_ZRBouton_x + largeur_ZRBouton) && y > fromIntegral position_ZRBouton_y  && y < fromIntegral (position_ZRBouton_y + hauteur_ZRBouton)
                            then gs { displayText = Just "build ZRType" , selectedZone = Just ZRType }
                            else gs
                     Nothing -> gs


handleMouseClick_touche :: GameState -> GameState
handleMouseClick_touche gs =
  let (pressed,pos) = mouse_state gs
      px = fromIntegral $ persoX gs
      py = fromIntegral $ persoY gs
  in case pressed of
        False -> gs
        True -> case pos of
                     Just (P (V2 x y)) ->
                            if  x > px  && x < px + 100 && y > py  && y < py + 100
                            then gs { displayText = Just "Touché!" }
                            else gs
                     Nothing -> gs
