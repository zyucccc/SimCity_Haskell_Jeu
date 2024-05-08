module Entitys.Entitys where

import  Maps.Formes (Forme,Coord)

------------------------------------------
-- Batiments
------------------------------------------
data Batiment
  = Cabane Forme Coord Int [CitId]
  | Atelier Forme Coord Int [CitId]
  | Epicerie Forme Coord Int [CitId]
  | Commissariat Forme Coord

newtype BatId =  BatId Int deriving (Eq, Ord)

instance Show BatId where
  show (BatId id) = show id


data BatimentType = CabaneType | AtelierType | EpicerieType | CommissariatType deriving (Eq, Ord)

instance Show BatimentType where
      show CabaneType = "Cabane"
      show AtelierType = "Atelier"
      show EpicerieType = "Epicerie"
      show CommissariatType = "Commissariat"


------------------------------------------
-- Citoyens
------------------------------------------
newtype Occupation = O String

data Citoyen = Immigrant Coord (Int , Int , Int) Occupation
             | Habitant Coord (Int , Int , Int) (BatId , Maybe BatId , Maybe BatId) Occupation
             | Emigrant Coord Occupation

newtype CitId =  CitId String

instance Show CitId where
  show (CitId id) = show id