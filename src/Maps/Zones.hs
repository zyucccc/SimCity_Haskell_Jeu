module Maps.Zones where

import Foreign.C.Types (CInt (..) )

import  Maps.Formes
import Config.Config
import  Entitys.Entitys (Batiment,BatId,CitId,Citoyen)

data Direction_Route = Vertical | Horizontal deriving (Eq, Ord)

instance Show Direction_Route where
      show Vertical = "Vertical"
      show Horizontal = "Horizontal"


data Zone
  = Eau Forme
  | Grass Forme
  | Terre Forme
  | Route Forme Direction_Route
  | ZR Forme [Batiment]
  | ZI Forme [Batiment]
  | ZC Forme [Batiment]
  | Admin Forme Batiment

data ZoneType = EauType | GrassType | TerreType | RouteType_Vertical | RouteType_Horizontal | ZRType | ZIType | ZCType | AdminType deriving (Eq, Ord)

instance Show ZoneType where
      show EauType = "Eau"
      show GrassType = "Grass"
      show TerreType = "Terre"
      show RouteType_Vertical = "Route Vertical"
      show RouteType_Horizontal = "Route Horizontal"
      show ZRType = "Zone Residentielle"
      show ZIType = "Zone Industrielle"
      show ZCType = "Zone Commerciale"
      show AdminType = "Administration"


instance Show Zone where
    show (Eau f) = "Eau " ++ show f
    show (Grass f) = "Grass " ++ show f
    show (Terre f) = "Terre " ++ show f
    show (Route f dir) = "Route " ++ show dir ++ show f
    show (ZR f _) = "Zone Residentielle " ++ show f
    show (ZI f _) = "Zone Industrielle " ++ show f
    show (ZC f _) = "Zone Commerciale " ++ show f
    show (Admin f _) = "Administration " ++ show f

instance Eq Zone where
    (Eau f) == (Eau f') = f == f'
    (Route f dir) == (Route f' dir') = f == f' && dir == dir'
    (ZR f _) == (ZR f' _) = f == f'
    (ZI f _) == (ZI f' _) = f == f'
    (ZC f _) == (ZC f' _) = f == f'
    (Admin f _) == (Admin f' _) = f == f'
    _ == _ = False

newtype ZoneId =  ZoneId CInt deriving (Eq, Ord)

instance Show ZoneId where
  show (ZoneId id) = show id


-- accept une zone et renvoie sa forme.
zoneForme :: Zone -> Forme
zoneForme (Eau f) = f
zoneForme (Grass f) = f
zoneForme (Terre f) = f
zoneForme (Route f _) = f
zoneForme (ZR f _) = f
zoneForme (ZI f _) = f
zoneForme (ZC f _) = f
zoneForme (Admin f _) = f


-- accept une zone et renvoie la liste des cases qu'elle occupe.
zoneCases :: Zone -> [Coord]
zoneCases zone = formeCases (zoneForme zone)

--renvoie la coordonnee de la zone
zoneCoord :: Zone -> Coord
zoneCoord zone = coord_forme $ zoneForme zone

createZone_ZR :: Coord -> Zone
createZone_ZR (C x y) = ZR (Rectangle (C x y) largeur_ZR hauteur_ZR) []

createZone_ZI :: Coord -> Zone
createZone_ZI (C x y) = ZI (Rectangle (C x y) largeur_ZI hauteur_ZI) []

createZone_ZC :: Coord -> Zone
createZone_ZC (C x y) = ZC (Rectangle (C x y) largeur_ZC hauteur_ZC) []

createZone_Admin :: Coord -> Batiment -> Zone
createZone_Admin (C x y) bat = Admin (Rectangle (C x y) largeur_Admin hauteur_Admin) bat

createZone_Route :: Coord -> Direction_Route -> Zone
createZone_Route (C x y) dir = Route (Rectangle (C x y) (largeur_route `div` 2) (hauteur_route `div` 2)) dir



