module Model where

import SDL

import GHC.Int (Int32)
import Foreign.C.Types (CInt)
import Events.Keyboard (Keyboard)
import qualified Events.Keyboard as K
import qualified Data.List as List
import Events.Mouse (MouseState)
import qualified Events.Mouse as MOS

import Maps.Formes
import Maps.Monde
import Maps.Zones
import Entitys.Entitys
import Entitys.Ville

import Debug.Trace (trace)
import Config.Config
import qualified Data.Map as Map

data Economic = Economic {
  money :: CInt
} deriving (Show)


initEconomic :: Economic
initEconomic = Economic 8000  -- init economic == 8000


data GameState = GameState { mouse_state :: MouseState
                           , displayText :: Maybe String
                           , monde :: Monde
                           , selectedZone :: Maybe ZoneType
                           , ville :: Ville
                           , nextZoneId :: ZoneId
                           , economic :: Economic
                           , economicUpdateTime :: Float
                           }
                           deriving (Show)

inv_GameState :: GameState -> Bool
inv_GameState gs = inv_economic_gameState gs

inv_economic_gameState :: GameState -> Bool
inv_economic_gameState gs = let Economic val = (economic gs) in val >= 0


-- init game state
initGameState :: Monde -> GameState
initGameState monde = GameState (False, Nothing) Nothing monde Nothing initVille (ZoneId 1) initEconomic 0

-- update gamestate(economic) every 3s en fonction du nombre de zones commerciales

updateEconomic :: GameState -> GameState
updateEconomic gs =
  let currentMoney = money (economic gs)
      zcCount = countZC (monde gs)
      newMoney = currentMoney + fromIntegral zcCount * 10
  in gs { economic = (economic gs) { money = newMoney }, economicUpdateTime = 0 }

-- calcul le nb de zones commerciales actuelles
countZC :: Monde -> Int
countZC monde =
    let zcZones = filter isZC $ Map.elems monde
        zcCount = length zcZones `div` 4 -- div 4 car 4 cases pour une zone commerciale
     in zcCount


isZC :: Maybe Zone -> Bool
isZC (Just (ZC _ _)) = True
isZC _ = False

-- game loop, update game state every frame (mouse state+keyboard state)
gameStep :: RealFrac a => GameState -> Keyboard -> MouseState -> a -> GameState
gameStep gstate kbd mos deltaTime =
  let gstate' = (if K.keypressed KeycodeE kbd
               then handleClavierE else id)
              .
              (if K.keypressed KeycodeV kbd
               then handleClavierV else id)
              .
              (if K.keypressed KeycodeB kbd
               then handleClavierB else id)
              .
              (if K.keypressed KeycodeT kbd
               then handleClavierT else id)
              .
              (if MOS.mousePressed mos
               then handleMouseClick else id)
              $ gstate

      newEconomicUpdateTime = economicUpdateTime gstate' + realToFrac deltaTime
-- chaque 3s, update economic (en fonction de nb de zones commerciales)
      gstate'' = if newEconomicUpdateTime >= 3 --3s
                 then updateEconomic gstate'
                 else gstate' { economicUpdateTime = newEconomicUpdateTime }
  in gstate''

--handle events des mouses
handleMouseClick :: GameState -> GameState
handleMouseClick gs = handleMouseClick_BatimentType $ handleMouseClick_BuildZone gs

-- hendle mouse click pour construire des zones
handleMouseClick_BuildZone :: GameState -> GameState
handleMouseClick_BuildZone gs =
  let (pressed, pos) = mouse_state gs
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
                                        Just ZRType -> case pre_DejaBuild_Monde ZRType coord_pixel notre_monde of
                                            True -> gs
                                            False -> let new_zone = createZone_ZR coord_case
                                                         Economic money_actuel = economic gs
                                                         Economic money_afterBuild = Economic (money_actuel - 300)
                                                     in if money_afterBuild  < 0 then gs { displayText = Just "Pas assez d'argent!" }
                                                        else gs {monde = placeZone new_zone notre_monde, ville = addZone_Ville (ZoneId id) new_zone (ville gs), nextZoneId = ZoneId (id + 1), economic = Economic money_afterBuild, displayText = Just ("build ZRType in " ++ show (ville gs)) }
                                        Just ZIType -> case pre_DejaBuild_Monde ZIType coord_pixel notre_monde of
                                            True -> gs
                                            False -> let new_zone = createZone_ZI coord_case
                                                         Economic money_actuel = economic gs
                                                         Economic money_afterBuild = Economic (money_actuel - 600)
                                                     in if money_afterBuild  < 0 then gs { displayText = Just "Pas assez d'argent!" }
                                                        else gs {monde = placeZone new_zone notre_monde, ville = addZone_Ville (ZoneId id) new_zone (ville gs), nextZoneId = ZoneId (id + 1), economic = Economic money_afterBuild, displayText = Just ("build ZIType in " ++ show (ville gs)) }
                                        Just ZCType -> case pre_DejaBuild_Monde ZCType coord_pixel notre_monde of
                                            True -> gs
                                            False -> let new_zone = createZone_ZC coord_case
                                                         Economic money_actuel = economic gs
                                                         Economic money_afterBuild = Economic (money_actuel - 500)
                                                     in if money_afterBuild  < 0 then gs { displayText = Just "Pas assez d'argent!" }
                                                        else gs {monde = placeZone new_zone notre_monde, ville = addZone_Ville (ZoneId id) new_zone (ville gs), nextZoneId = ZoneId (id + 1), economic = Economic money_afterBuild, displayText = Just ("build ZCType in " ++ show (ville gs)) }
                                        Just AdminType -> case pre_DejaBuild_Monde AdminType coord_pixel notre_monde of
                                            True -> gs
                                            False -> let new_zone = createZone_Admin coord_case (Commissariat coord_case)
                                                         Economic money_actuel = economic gs
                                                         Economic money_afterBuild = Economic (money_actuel - 600)
                                                     in if money_afterBuild  < 0 then gs { displayText = Just "Pas assez d'argent!" }
                                                        else gs {monde = placeZone new_zone notre_monde, ville = addZone_Ville (ZoneId id) new_zone (ville gs), nextZoneId = ZoneId (id + 1), economic = Economic money_afterBuild, displayText = Just ("build AdminType in " ++ show (ville gs)) }
                                        Just RouteType_Vertical -> case pre_DejaBuild_Monde RouteType_Vertical coord_pixel notre_monde of
                                            True -> gs
                                            False -> let new_zone = createZone_Route coord_case Vertical
                                                         Economic money_actuel = economic gs
                                                         Economic money_afterBuild = Economic (money_actuel - 100)
                                                      in if money_afterBuild  < 0 then gs { displayText = Just "Pas assez d'argent!" }
                                                         else gs {monde = placeZone new_zone notre_monde, ville = addZone_Ville (ZoneId id) new_zone (ville gs), nextZoneId = ZoneId (id + 1), economic = Economic money_afterBuild, displayText = Just ("build RouteType_Vertical in " ++ show (ville gs)) }
                                        Just RouteType_Horizontal -> case pre_DejaBuild_Monde RouteType_Horizontal coord_pixel notre_monde of
                                            True -> gs
                                            False -> let new_zone = createZone_Route coord_case Horizontal
                                                         Economic money_actuel = economic gs
                                                         Economic money_afterBuild = Economic (money_actuel - 100)
                                                      in if money_afterBuild  < 0 then gs { displayText = Just "Pas assez d'argent!" }
                                                         else gs {monde = placeZone new_zone notre_monde, ville = addZone_Ville (ZoneId id) new_zone (ville gs), nextZoneId = ZoneId (id + 1), economic = Economic money_afterBuild, displayText = Just ("build RouteType_Horizontal in " ++ show (ville gs)) }
                                        Just CentraleType -> case pre_DejaBuild_Monde CentraleType coord_pixel notre_monde of
                                            True -> gs
                                            False -> let new_zone = createZone_Centrale coord_case
                                                         Economic money_actuel = economic gs
                                                         Economic money_afterBuild = Economic (money_actuel - 800)
                                                      in if money_afterBuild  < 0 then gs { displayText = Just "Pas assez d'argent!" }
                                                         else gs {monde = placeZone new_zone notre_monde, ville = addZone_Ville (ZoneId id) new_zone (ville gs), nextZoneId = ZoneId (id + 1), economic = Economic money_afterBuild, displayText = Just ("build CentraleType in " ++ show (ville gs)) }
                                        Just CableType -> case pre_DejaBuild_Monde CableType coord_pixel notre_monde of
                                            True -> gs
                                            False -> let new_zone = createZone_Cable coord_case
                                                         Economic money_actuel = economic gs
                                                         Economic money_afterBuild = Economic (money_actuel - 50)
                                                      in if money_afterBuild  < 0 then gs { displayText = Just "Pas assez d'argent!" }
                                                         else if not (pre_Voisin_Cable coord_pixel notre_monde) then gs { displayText = Just "Pas de centrale ou cable à coté,vous ne peuvez pas construire les cables ici" }
                                                         else gs {monde = placeZone new_zone notre_monde, ville = addZone_Ville (ZoneId id) new_zone (ville gs), nextZoneId = ZoneId (id + 1), economic = Economic money_afterBuild, displayText = Just ("build CableType in " ++ show (ville gs)) }
                                        _ -> gs
                     Nothing -> gs

-- handle event choisir les type de zones depuis
handleMouseClick_BatimentType :: GameState -> GameState
handleMouseClick_BatimentType gs =
  let (pressed, pos) = mouse_state gs
  in case pressed of
        False -> gs
        True -> case pos of
                     Just (P (V2 x y)) ->
                            if x > fromIntegral position_ZRBouton_x && x < fromIntegral (position_ZRBouton_x + largeur_Bouton) && y > fromIntegral position_ZRBouton_y && y < fromIntegral (position_ZRBouton_y + hauteur_Bouton)
                            then gs { displayText = Just "choose ZRType", selectedZone = Just ZRType }
                            else if x > fromIntegral position_ZIBouton_x && x < fromIntegral (position_ZIBouton_x + largeur_Bouton) && y > fromIntegral position_ZIBouton_y && y < fromIntegral (position_ZIBouton_y + hauteur_Bouton)
                            then gs { displayText = Just "choose ZIType", selectedZone = Just ZIType }
                            else if x > fromIntegral position_ZCBouton_x && x < fromIntegral (position_ZCBouton_x + largeur_Bouton) && y > fromIntegral position_ZCBouton_y && y < fromIntegral (position_ZCBouton_y + hauteur_Bouton)
                            then gs { displayText = Just "choose ZCType", selectedZone = Just ZCType }
                            else if x > fromIntegral position_ADBouton_x && x < fromIntegral (position_ADBouton_x + largeur_Bouton) && y > fromIntegral position_ADBouton_y && y < fromIntegral (position_ADBouton_y + hauteur_Bouton)
                            then gs { displayText = Just "choose AdminType", selectedZone = Just AdminType }
                            else if x > fromIntegral position_routeBoutonVerticale_x && x < fromIntegral (position_routeBoutonVerticale_x + largeur_Bouton) && y > fromIntegral position_routeBoutonVerticale_y && y < fromIntegral (position_routeBoutonVerticale_y + hauteur_Bouton)
                            then gs { displayText = Just "choose Route Verticale", selectedZone = Just RouteType_Vertical }
                            else if x > fromIntegral position_routeBoutonHoriz_x && x < fromIntegral (position_routeBoutonHoriz_x + largeur_Bouton) && y > fromIntegral position_routeBoutonHoriz_y && y < fromIntegral (position_routeBoutonHoriz_y + hauteur_Bouton)
                            then gs { displayText = Just "choose Route Horizontale", selectedZone = Just RouteType_Horizontal }
                            else if x > fromIntegral position_centrale_x && x < fromIntegral (position_centrale_x + largeur_Bouton) && y > fromIntegral position_centrale_y && y < fromIntegral (position_centrale_y + hauteur_Bouton)
                            then gs { displayText = Just "choose Centrale", selectedZone = Just CentraleType }
                            else if x > fromIntegral position_cable_x && x < fromIntegral (position_cable_x + largeur_Bouton) && y > fromIntegral position_cable_y && y < fromIntegral (position_cable_y + hauteur_Bouton)
                            then gs { displayText = Just "choose Cable", selectedZone = Just CableType }
                            else gs
                     Nothing -> gs

-- Clavier
-- afficher l'argent actuel
handleClavierE :: GameState -> GameState
handleClavierE gs = gs { displayText = Just ("Money actuel: " ++ show(economic gs)) }

-- afficher la ville actuel
handleClavierV :: GameState -> GameState
handleClavierV gs = gs { displayText = Just ("Ville actuel: " ++ show(ville gs)) }

-- build les batiments dans le zones
handleClavierB :: GameState -> GameState
handleClavierB gs =
   let (pressed, pos) = mouse_state gs
   in
   case pos of
     Just (P (V2 x y)) ->
       let coord = C (fromIntegral x) (fromIntegral y)
       in
         case getZoneByCoord coord (monde gs) of
           Nothing -> gs
           -- faut que la zone est lié au electricité avec cable pour build des batiments
           Just zone -> if checkZone_Electrique zone (monde gs) then
                          let
                             ZoneId id = getZoneIdByZone zone (ville gs)
                             zone_Coord = zoneCoord zone
                             monde' = monde gs
                             ville' = ville gs
                          in
                          case zone of
                            ZR f batiments ->let zone' = ((buildBatiment zone (Cabane zone_Coord id []))) in gs { monde = Map.insert zone_Coord (Just zone') monde', displayText = Just "build un nouveau Cabane" ,ville = ville' { viZones = Map.insert (ZoneId id) zone' (viZones ville') } }
                            ZI f batiments ->let zone' = ((buildBatiment zone (Atelier zone_Coord id []))) in gs { monde = Map.insert zone_Coord (Just zone') monde', displayText = Just "build un nouveau Atelier" ,ville = ville' { viZones = Map.insert (ZoneId id) zone' (viZones ville') } }
                            ZC f batiments ->let zone' = ((buildBatiment zone (Epicerie zone_Coord id []))) in gs { monde = Map.insert zone_Coord (Just zone') monde', displayText = Just "build un nouveau Epicerie" ,ville = ville' { viZones = Map.insert (ZoneId id) zone' (viZones ville') } }
                            _ -> gs
                          else gs{displayText = Just "Pas de electricité, vous ne peuvez pas construire les batiments ici"}

-- afficher les nb de batiments totals
handleClavierT :: GameState -> GameState
handleClavierT gs =
  let zones = Map.toList (viZones $ ville gs)
      zoneInfo = foldl (\acc (id, zone) -> case zone of
                                       ZR _ batiments -> acc ++ "\nZone Résidentielle, id: " ++ show id ++ ", nombre de batiments: " ++ show (length batiments)
                                       ZI _ batiments -> acc ++ "\nZone Industrielle, id: " ++ show id ++ ", nombre de batiments: " ++ show (length batiments)
                                       ZC _ batiments -> acc ++ "\nZone Commerciale, id: " ++ show id ++ ", nombre de batiments: " ++ show (length batiments)
                                       _ -> acc) "" zones
  in gs { displayText = Just zoneInfo }


