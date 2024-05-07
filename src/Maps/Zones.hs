module Maps.Zones where

import Foreign.C.Types (CInt (..) )

import  Maps.Formes
import  Entitys.Entitys (Batiment,BatId,CitId,Citoyen)

data Zone
  = Eau Forme
  | Grass Forme
  | Terre Forme
  | Route Forme
  | ZR Forme [Batiment]
  | ZI Forme [Batiment]
  | ZC Forme [Batiment]
  | Admin Forme Batiment

instance Show Zone where
    show (Eau f) = "Eau " ++ show f
    show (Grass f) = "Grass " ++ show f
    show (Terre f) = "Terre " ++ show f
    show (Route f) = "Route " ++ show f
    show (ZR f _) = "Zone Residentielle " ++ show f
    show (ZI f _) = "Zone Industrielle " ++ show f
    show (ZC f _) = "Zone Commerciale " ++ show f
    show (Admin f _) = "Administration " ++ show f

instance Eq Zone where
    (Eau f) == (Eau f') = f == f'
    (Route f) == (Route f') = f == f'
    (ZR f _) == (ZR f' _) = f == f'
    (ZI f _) == (ZI f' _) = f == f'
    (ZC f _) == (ZC f' _) = f == f'
    (Admin f _) == (Admin f' _) = f == f'
    _ == _ = False

newtype ZoneId =  ZoneId Int deriving (Eq, Ord)

-- accept une zone et renvoie sa forme.
zoneForme :: Zone -> Forme
zoneForme (Eau f) = f
zoneForme (Route f) = f
zoneForme (ZR f _) = f
zoneForme (ZI f _) = f
zoneForme (ZC f _) = f
zoneForme (Admin f _) = f

-- accept une zone et renvoie la liste des cases qu'elle occupe.
zoneCases :: Zone -> [Coord]
zoneCases zone = formeCases (zoneForme zone)





