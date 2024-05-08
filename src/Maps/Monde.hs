module Maps.Monde where

import Maps.Formes
import Maps.Zones
import qualified Data.Map as Map
import Config.Config
import Foreign.C.Types (CInt (..) )
import Control.Monad (forM)
import System.Random (randomRIO)

type Monde = Map.Map Coord (Maybe Zone)

-- initMap
--initMonde :: CInt -> CInt -> Monde
--initMonde pixelWidth pixelHeight = Map.fromList [ (C x y, Just (Terre (Rectangle (C x y) caseSize caseSize))) | x <- [0..cols-1], y <- [0..rows-1]]
initMonde :: CInt -> CInt -> IO Monde
initMonde pixelWidth pixelHeight = do
    cells <- forM [0..cols-1] $ \x ->
                forM [0..rows-1] $ \y -> do
                    r <- randomRIO (0, 30::CInt)
--                    let x' = x * caseSize
--                        y' = y * caseSize
                    let zone = case r of
                            0 -> Eau (Rectangle (C x y) caseSize caseSize)     -- 1% 的几率是水
                            1 -> Grass (Rectangle (C x y) caseSize caseSize)    -- 1% 的几率是草地
                            _ -> Terre (Rectangle (C x y) caseSize caseSize)    -- 其余是土地
                    return (C x y, Just zone)
    return $ Map.fromList (concat cells)

-- verifier si il y a deja une zone construit sur l'ensemble des cases
check_DejaBuild_Monde :: ZoneType -> Coord -> Monde -> Bool
check_DejaBuild_Monde zoneType (C x y) monde =
  let (C coord_x coord_y) = coordToRowCol (C x y)
      (largeur, hauteur) = case zoneType of
                            ZRType -> (largeur_ZR `div` caseSize, hauteur_ZR `div` caseSize)
                            _ -> (caseSize `div` caseSize, caseSize `div` caseSize)
      coords = [C (coord_x + dx) (coord_y + dy) | dx <- [0..largeur], dy <- [0..hauteur]]
  in any (\coord -> case Map.lookup coord monde of
                      Just (Just (Eau _)) -> True
                      Just (Just (ZR _ _)) -> True
                      Just (Just (ZI _ _)) -> True
                      Just (Just (ZC _ _)) -> True
                      Just (Just (Admin _ _)) -> True
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
    _ -> False

-- placer une forme sur la map
placeZone :: Zone -> Monde -> Monde
placeZone zone monde = Map.union (Map.fromList $ zip (zoneCases zone) (repeat (Just zone))) monde
--placeZone zone monde = Map.union (Map.fromList $ zip (repeat (zoneCoord zone)) (repeat (Just zone))) monde
