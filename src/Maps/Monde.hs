module Maps.Monde where

import Maps.Formes
import qualified Data.Map as Map

import Foreign.C.Types (CInt (..) )

type Monde = Map.Map Coord (Maybe Forme)

-- initMap
initMap :: CInt -> CInt -> Monde
initMap width height = Map.fromList [((C x y), Nothing) | x <- [0..width-1], y <- [0..height-1]]

-- placer une forme sur la map
placeForme :: Forme -> Monde -> Monde
placeForme forme monde = foldr (\coord acc -> Map.insert coord (Just forme) acc) monde (tous_coords_forme forme)