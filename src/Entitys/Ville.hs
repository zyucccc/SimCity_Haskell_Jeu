module Entitys.Ville where

import Data.Map (Map)
import qualified Data.Map as M

import Maps.Formes (collision)
import Maps.Zones (Zone, ZoneId,zoneForme)
import Entitys.Entitys (CitId, Citoyen)


data Ville = Ville {viZones :: Map ZoneId Zone ,viCit :: Map CitId Citoyen}


prop_ville_sansCollision :: Ville -> Bool
prop_ville_sansCollision (Ville zones _) = all (\(z1,z2) -> not (collision (zoneForme z1) (zoneForme z2))) [(z1,z2) | z1 <- M.elems zones, z2 <- M.elems zones, z1 /= z2]