module Maps.Zones where

import  Maps.Formes (Forme,Coord)
import  Entitys.Entitys (Batiment,BatId,CitId,Citoyen)

data Zone
  = Eau Forme
  | Route Forme
  | ZR Forme [Batiment]
  | ZI Forme [Batiment]
  | ZC Forme [Batiment]
  | Admin Forme Batiment

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



