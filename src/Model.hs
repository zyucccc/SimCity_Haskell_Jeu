
module Model where

import SDL

import GHC.Int (Int32)
import Foreign.C.Types (CInt)
import Events.Keyboard (Keyboard)
import qualified Events.Keyboard as K

import Events.Mouse (MouseState)
import qualified Events.Mouse as MOS

import Maps.Formes
import Maps.Monde
import Maps.Zones
import Entitys.Entitys
import Entitys.Ville

import Debug.Trace (trace)
import Config.Config

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int
                           , mouse_state :: MouseState
                           , displayText :: Maybe String
                           , monde :: Monde
                           , selectedZone :: Maybe ZoneType
                           , ville :: Ville
                           , nextZoneId :: ZoneId
                           }
                           deriving (Show)


initGameState :: Monde -> GameState
initGameState monde = GameState 200 300 4 (False,Nothing) Nothing monde Nothing initVille (ZoneId 0)

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
                       if x > fromIntegral position_Toolbox_x && x < fromIntegral (position_Toolbox_x + largeur_Toolbox) && y > fromIntegral position_Toolbox_y && y < fromIntegral(position_Toolbox_y + hauteur_Toolbox)
                        then gs
                        else
                            let coord_pixel = C (fromIntegral x) (fromIntegral y)
                                coord_case = coordToRowCol coord_pixel
                                ZoneId id = nextZoneId gs
                                in case selectedZone gs of
                                        Just ZRType -> case check_DejaBuild_Monde ZRType coord_pixel notre_monde of
                                            True -> gs
                                            False -> let new_zone = createZone_ZR coord_case in gs {monde = placeZone new_zone notre_monde, ville = addZone_Ville (ZoneId id) new_zone (ville gs), displayText = Just ("build ZRType in "++show (ville gs)), nextZoneId = ZoneId (id + 1) }
                                        Just ZIType -> case check_DejaBuild_Monde ZIType coord_pixel notre_monde of
                                            True -> gs
                                            False -> let new_zone = createZone_ZI coord_case in gs {monde = placeZone new_zone notre_monde, ville = addZone_Ville (ZoneId id) new_zone (ville gs), nextZoneId = ZoneId (id + 1), displayText = Just ("build ZIType in "++ show (ville gs)) }
                                        Just ZCType -> case check_DejaBuild_Monde ZCType coord_pixel notre_monde of
                                            True -> gs
                                            False -> let new_zone = createZone_ZC coord_case in gs {monde = placeZone new_zone notre_monde, ville = addZone_Ville (ZoneId id) new_zone (ville gs), nextZoneId = ZoneId (id + 1), displayText = Just ("build ZCType in "++ show (ville gs)) }
                                        Just AdminType -> case check_DejaBuild_Monde AdminType coord_pixel notre_monde of
                                            True -> gs
                                            False -> let new_zone = createZone_Admin coord_case (Commissariat (Maps.Formes.Rectangle (C (fromIntegral x) (fromIntegral y)) (fromIntegral largeur_Admin) (fromIntegral hauteur_Admin)) coord_case) in gs {monde = placeZone new_zone notre_monde, ville = addZone_Ville (ZoneId id) new_zone (ville gs), nextZoneId = ZoneId (id + 1), displayText = Just ("build AdminType"++ show (ville gs)) }
                                        Just RouteType_Vertical -> case check_DejaBuild_Monde RouteType_Vertical coord_pixel notre_monde of
                                            True -> gs
                                            False -> let new_zone = createZone_Route coord_case Vertical in gs {monde = placeZone new_zone notre_monde, ville = addZone_Ville (ZoneId id) new_zone (ville gs), nextZoneId = ZoneId (id + 1), displayText = Just ("build RouteType_Vertical in "++ show (ville gs)) }
                                        Just RouteType_Horizontal -> case check_DejaBuild_Monde RouteType_Horizontal coord_pixel notre_monde of
                                            True -> gs
                                            False -> let new_zone = createZone_Route coord_case Horizontal in gs {monde = placeZone new_zone notre_monde, ville = addZone_Ville (ZoneId id) new_zone (ville gs), nextZoneId = ZoneId (id + 1), displayText = Just ("build RouteType_Horizontal in "++ show (ville gs)) }
                                        _ -> gs
                     Nothing -> gs


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
                            else if x > fromIntegral position_ZIBouton_x  && x < fromIntegral (position_ZIBouton_x + largeur_ZIBouton) && y > fromIntegral position_ZIBouton_y  && y < fromIntegral (position_ZIBouton_y + hauteur_ZIBouton)
                            then gs { displayText = Just "build ZIType" , selectedZone = Just ZIType }
                            else if  x > fromIntegral position_ZCBouton_x  && x < fromIntegral (position_ZCBouton_x + largeur_ZCBouton) && y > fromIntegral position_ZCBouton_y  && y < fromIntegral (position_ZCBouton_y + hauteur_ZCBouton)
                            then gs { displayText = Just "build ZCType" , selectedZone = Just ZCType }
                            else if  x > fromIntegral position_ADBouton_x  && x < fromIntegral (position_ADBouton_x + largeur_ADBouton) && y > fromIntegral position_ADBouton_y  && y < fromIntegral (position_ADBouton_y + hauteur_ADBouton)
                            then gs { displayText = Just "build AdminType" , selectedZone = Just AdminType }
                            else if  x > fromIntegral position_routeBoutonVerticale_x  && x < fromIntegral (position_routeBoutonVerticale_x + largeur_routeBouton) && y > fromIntegral position_routeBoutonVerticale_y  && y < fromIntegral (position_routeBoutonVerticale_y + hauteur_routeBouton)
                            then gs { displayText = Just "build Route Verticale" , selectedZone = Just RouteType_Vertical }
                            else if x > fromIntegral position_routeBoutonHoriz_x  && x < fromIntegral (position_routeBoutonHoriz_x + largeur_routeBouton) && y > fromIntegral position_routeBoutonHoriz_y  && y < fromIntegral (position_routeBoutonHoriz_y + hauteur_routeBouton)
                            then gs { displayText = Just "build Route Horizontale " , selectedZone = Just RouteType_Horizontal }
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
