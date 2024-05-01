module Citoyens where

import Maps.Formes (Coord)
import Zones (BatId)

newtype Occupation = O String 


data Citoyen = Immigrant Coord (Int , Int , Int) Occupation
             | Habitant Coord (Int , Int , Int) (BatId , Maybe BatId , Maybe BatId) Occupation
             | Emigrant Coord Occupation