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
--假设全是土地
-- 假设全是土地
--initMonde :: CInt -> CInt -> Monde
--initMonde pixelWidth pixelHeight = Map.fromList [ (C x y, Just (Terre (Rectangle (C x y) caseSize caseSize))) | x <- [0..cols-1], y <- [0..rows-1]]
initMonde :: CInt -> CInt -> IO Monde
initMonde pixelWidth pixelHeight = do
    cells <- forM [0..cols-1] $ \x ->
                forM [0..rows-1] $ \y -> do
                    r <- randomRIO (0, 30::CInt)
                    let zone = case r of
                            0 -> Eau (Rectangle (C x y) caseSize caseSize)     -- 1% 的几率是水
                            1 -> Grass (Rectangle (C x y) caseSize caseSize)    -- 1% 的几率是草地
                            _ -> Terre (Rectangle (C x y) caseSize caseSize)    -- 其余是土地
                    return (C x y, Just zone)
    return $ Map.fromList (concat cells)

-- placer une forme sur la map
placeZone :: Zone -> Monde -> Monde
placeZone zone monde = Map.union (Map.fromList $ zip (zoneCases zone) (repeat (Just zone))) monde
