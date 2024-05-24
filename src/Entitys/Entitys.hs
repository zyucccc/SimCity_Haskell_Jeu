module Entitys.Entitys where

import Foreign.C.Types (CInt (..) )
import  Maps.Formes (Forme,Coord)

------------------------------------------
-- Batiments
------------------------------------------
data Batiment
  = Cabane Coord CInt [CitId]
  | Atelier Coord CInt [CitId]
  | Epicerie Coord CInt [CitId]
  | Commissariat Coord

-- nous imposons que les cabane ne peut que construire que dans les zones residentielles
-- les ateliers dans les zones industrielles
-- les epicerie dans les zones commerciales
-- les commissariats dans les zones administratives

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

data Citoyen = Immigrant Coord (CInt , CInt , CInt) (Maybe Occupation)
             | Habitant Coord (CInt , CInt , CInt) (BatId , Maybe BatId , Maybe BatId) (Maybe Occupation)
             | Emigrant Coord (Maybe Occupation)

newtype CitId =  CitId String

instance Show CitId where
  show (CitId id) = show id