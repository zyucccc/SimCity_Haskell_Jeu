module Maps.Monde where

import Maps.Formes
import Maps.Zones
import qualified Data.Map as Map
import Config.Config
import Foreign.C.Types (CInt (..) )
import Control.Monad (forM)
import System.Random (randomRIO)

-- Monde est une carte de coordonnées à des zones
type Monde = Map.Map Coord (Maybe Zone)

inv_Coord_Monde :: Monde -> Bool
inv_Coord_Monde monde = all (\(coord, _) -> case coord of
                                              C x y -> x >= 0 && y >= 0 && x<= (window_largeur `div` caseSize) && y <= (window_hauteur `div` caseSize)) (Map.toList monde)

-- initMap avec random grass, terre, eau
initMonde :: CInt -> CInt -> IO Monde
initMonde pixelWidth pixelHeight = do
    cells <- forM [0..cols-1] $ \x ->
                forM [0..rows-1] $ \y -> do
                    r <- randomRIO (0, 30::CInt)
                    let zone = case r of
                            0 -> Eau (Rectangle (C x y) caseSize caseSize)
                            1 -> Grass (Rectangle (C x y) caseSize caseSize)
                            _ -> Terre (Rectangle (C x y) caseSize caseSize)
                    return (C x y, Just zone)
    return $ Map.fromList (concat cells)

-- initMap pure
initMondePure :: CInt -> CInt -> Monde
initMondePure pixelWidth pixelHeight =
  let cells = [ ((C x y), Just (Terre (Rectangle (C x y) caseSize caseSize))) | x <- [0..cols-1], y <- [0..rows-1] ]
  in Map.fromList cells

-- verifier si il y a deja une zone construit sur l'ensemble des cases
pre_DejaBuild_Monde :: ZoneType -> Coord -> Monde -> Bool
pre_DejaBuild_Monde zoneType (C x y) monde =
  let (C coord_x coord_y) = coordToRowCol (C x y)
  --on calcul le largeur, hauteur de la zone correspondant,(nombre de cases)
  --on parcourt les cases de la zone, si une case est deja construite, on renvoie True
      (largeur, hauteur) = case zoneType of
                            ZRType -> (largeur_ZR `div` caseSize, hauteur_ZR `div` caseSize)
                            ZIType -> (largeur_ZI `div` caseSize, hauteur_ZI `div` caseSize)
                            ZCType -> (largeur_ZC `div` caseSize, hauteur_ZC `div` caseSize)
                            AdminType -> (largeur_Admin `div` caseSize, hauteur_Admin `div` caseSize)
                            CentraleType -> (largeur_Centrale `div` caseSize, hauteur_Centrale `div` caseSize)
                            CableType -> (0, 0)
                            RouteType_Vertical -> (0, 0)
                            RouteType_Horizontal -> (0, 0)
                            _ -> (caseSize `div` caseSize, caseSize `div` caseSize)
      coords = [C (coord_x + dx) (coord_y + dy) | dx <- [0..largeur], dy <- [0..hauteur]]
  in any (\coord -> case Map.lookup coord monde of
                      Just (Just (Eau _)) -> True
                      Just (Just (ZR _ _)) -> True
                      Just (Just (ZI _ _)) -> True
                      Just (Just (ZC _ _)) -> True
                      Just (Just (Admin _ _)) -> True
                      Just (Just (Route _ _)) -> True
                      Just (Just (Centrale _)) -> True
                      Just (Just (Cable _)) -> True
                      _ -> False) coords

-- verifier si cette case est voisine d'une case cable/centrale
pre_Voisin_Cable :: Coord -> Monde -> Bool
pre_Voisin_Cable coord monde =
  let (C x y) = coordToRowCol coord
      coords = [C (x + dx) (y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]
  in any (\coord -> case Map.lookup coord monde of
                      Just (Just (Centrale _)) -> True
                      Just (Just (Cable _)) -> True
                      _ -> False) coords


--verifier si le coord correspond à une case de Eau ou d'autre Batiment
checkCoord_Monde :: Coord -> Monde -> Bool
checkCoord_Monde coord_pixel monde =
  let coord_case = coordToRowCol coord_pixel
  in case Map.lookup coord_case monde of
    Just (Just (Eau _)) -> True
    Just (Just (ZR _ _)) -> True
    Just (Just (ZI _ _)) -> True
    Just (Just (ZC _ _)) -> True
    Just (Just (Admin _ _)) -> True
    Just (Just (Route _ _)) -> True
    Just (Just (Centrale _)) -> True
    Just (Just (Cable _)) -> True
    _ -> False

-- placer une forme sur la map
placeZone :: Zone -> Monde -> Monde
placeZone zone monde = Map.union (Map.fromList $ zip (zoneCases zone) (repeat (Just zone))) monde

-- post condition : la zone est bien placée
post_placeZone :: Zone -> Monde -> Monde -> Bool
post_placeZone zone monde monde_apres = Map.lookup (zoneCoord zone) monde_apres == Just (Just zone)

-- obtenir la zone by coord
getZoneByCoord :: Coord -> Monde -> Maybe Zone
getZoneByCoord coord monde =
  let coord_case = coordToRowCol coord
  in case Map.lookup coord_case monde of
    Just (Just zone) -> Just zone
    _ -> Nothing


-- verifier si la zone est lie avec une cable
pre_Zone_Electrique :: Zone -> Monde -> Bool
pre_Zone_Electrique zone monde =
  let zoneCoords = zoneCases zone
      surroundingCoords = concatMap (\(C x y) -> [C (x + dx) (y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]) zoneCoords
  in any (\coord -> case Map.lookup coord monde of
                      Just (Just (Cable _)) -> True
                      _ -> False) surroundingCoords



