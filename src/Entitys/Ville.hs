module Entitys.Ville where

import Data.Map (Map)
import qualified Data.Map as M

import Maps.Formes
import Maps.Zones
import Entitys.Entitys


data Ville = Ville {viZones :: Map ZoneId Zone ,viCit :: Map CitId Citoyen}

instance Show Ville where
    show (Ville zones cit) = "Ville {zones = " ++ show (M.keys zones) ++ ", citoyens = " ++ show (M.keys cit) ++ "}"

initVille :: Ville
initVille = Ville M.empty M.empty

addZone_Ville :: ZoneId -> Zone -> Ville -> Ville
addZone_Ville zid z (Ville zones cit) = Ville (M.insert zid z zones) cit

prop_ville_sansCollision :: Ville -> Bool
prop_ville_sansCollision (Ville zones _) = all (\(z1,z2) -> not (collision (zoneForme z1) (zoneForme z2))) [(z1,z2) | z1 <- M.elems zones, z2 <- M.elems zones, z1 /= z2]

