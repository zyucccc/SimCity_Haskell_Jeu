module Zones where

import  Formes (Forme,Coord)

data Batiment
  = Cabane Forme Coord Int [CitId]
  | Atelier Forme Coord Int [CitId]
  | Epicerie Forme Coord Int [CitId]
  | Commissariat Forme Coord

data Zone
  = Eau Forme
  | Route Forme
  | ZR Forme [Batiment]
  | ZI Forme [Batiment]
  | ZC Forme [Batiment]
  | Admin Forme Batiment

newtype ZoneId =  ZoneId Int deriving (Eq, Ord)

newtype BatId =  BatId Int deriving (Eq, Ord)

newtype CitId =  CitId String

-- accept une zone et renvoie sa forme.
zoneForme :: Zone -> Forme
zoneForme (Eau f) = f
zoneForme (Route f) = f
zoneForme (ZR f _) = f
zoneForme (ZI f _) = f
zoneForme (ZC f _) = f
zoneForme (Admin f _) = f



