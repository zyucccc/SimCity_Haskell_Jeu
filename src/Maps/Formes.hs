module Maps.Formes where

import Foreign.C.Types (CInt (..) )
import Data.List (nub)
import Config.Config

data Coord = C {cx :: CInt, cy :: CInt} deriving (Show , Eq)

instance Ord Coord where
    compare (C x y) (C x' y') = compare (x, y) (x', y')

data Forme = HSegment Coord CInt -- horizontal segment
           | VSegment Coord CInt -- vertical segment
           | Rectangle Coord CInt CInt -- rectangle segment
           deriving (Eq)
-- Hsegment : Coord : point de le plus à l’Ouest
-- Vsegment : Coord : point de le plus au Nord
-- Rectangle : Coord : point de le plus au Nord-Ouest,largeur,hauteur

instance Show Forme where
    show (HSegment (C x y) l) = "Hsegment(" <> show x <> "," <> show y <> ", length " <> show l <> ")"
    show (VSegment (C x y) l) = "Vsegment(" <> show x <> "," <> show y <> ", length " <> show l <> ")"
    show (Rectangle (C x y) l h) = "Rectangle(" <> show x <> "," <> show y <> ", largeur: " <> show l <> ", hauteur: " <> show h <> ")"


-- y:Nord -> y:Sud -> x:Ouest -> x:Est
limites :: Forme -> (CInt, CInt, CInt, CInt)
limites (HSegment (C x y) l) = (y, y, x, x + l)
limites (VSegment (C x y) l) = (y, y + l, x, x)
limites (Rectangle (C x y) l h) = (y, y + h, x, x + l)


--si la case aux coordonnees donnes appartient à la forme
appartient :: Coord -> Forme -> Bool
appartient (C x y) (HSegment (C x' y') l) = y == y' && x >= x' && x < x' + l
appartient (C x y) (VSegment (C x' y') l) = x == x' && y >= y' && y < y' + l
appartient (C x y) (Rectangle (C x' y') l h) = x >= x' && x < x' + l && y >= y' && y < y' + h


-- true si collision,mais erreur (faux positif) possible
collisionApprox :: Forme -> Forme -> Bool
collisionApprox f1 f2 = let (y1, y2, x1, x2) = limites f1
                            (y1', y2', x1', x2') = limites f2
                        in (x1 <= x2' && x2 >= x1' && y1 <= y2' && y2 >= y1')

--collision exacte
collision :: Forme -> Forme -> Bool
collision (HSegment (C x y) l) (HSegment (C x' y') l') = y == y' && x < x' + l' && x' < x + l
collision (HSegment (C x y) l) (VSegment (C x' y') l') = x' >= x && x' < x + l && y' <= y && y' + l' > y
collision (HSegment (C x y) l) (Rectangle (C x' y') l' h) = y >= y' && y < y' + h && x < x' + l' && x' < x + l
collision (VSegment (C x y) l) (HSegment (C x' y') l') = x >= x' && x < x' + l' && y' >= y && y' < y + l
collision (VSegment (C x y) l) (VSegment (C x' y') l') = x == x' && y < y' + l' && y' < y + l
collision (VSegment (C x y) l) (Rectangle (C x' y') l' h) = x >= x' && x < x' + l' && y < y' + h && y' < y + l
collision (Rectangle (C x y) l h) (HSegment (C x' y') l') = y' >= y && y' < y + h && x < x' + l' && x' < x + l
collision (Rectangle (C x y) l h) (VSegment (C x' y') l') = x' >= x && x' < x + l && y < y' + l' && y' < y + h
collision (Rectangle (C x y) l h) (Rectangle (C x' y') l' h') = x < x' + l' && x' < x + l && y < y' + h' && y' < y + h


--si la case aux coordonnees donnes est à cote de la forme
adjacent :: Coord -> Forme -> Bool
adjacent (C x y) (HSegment (C x' y') l) = 
    (y == y' && (x == x' - 1 || x == x' + l)) ||  -- left et right
    (x >= x' && x < x' + l && (y == y' + 1 || y == y' - 1))  -- haut et bas
adjacent (C x y) (VSegment (C x' y') l) =
    (x == x' && (y == y' - 1 || y == y' + l)) ||  -- haut et base
    (y >= y' && y < y' + l && (x == x' + 1 || x == x' - 1))  -- left et right
adjacent (C x y) (Rectangle (C x' y') w h) =
    (x >= x' && x < x' + w && (y == y' - 1 || y == y' + h)) ||  -- haut et bas
    (y >= y' && y < y' + h && (x == x' - 1 || x == x' + w))  -- left et right

--si la forme est à cote de la forme
adjacentes :: Forme -> Forme -> Bool
adjacentes f1 f2 = not (collision f1 f2) && any (flip adjacent f2) (bordure_forme f1)

--tous les cases des bordures de la forme
bordure_forme :: Forme -> [Coord]
bordure_forme (HSegment (C x y) l) = map (`C` y) [x .. x + l - 1]
bordure_forme (VSegment (C x y) l) = map (C x) [y .. y + l - 1]
bordure_forme (Rectangle (C x y) w h) = concat [
    map (`C` y) [x .. x + w - 1],                      -- bordure en haut
    map (`C` (y + h - 1)) [x .. x + w - 1],            -- bordure en bas
    map (C x) [y + 1 .. y + h - 2],                    -- bordure en gauche
    map (C (x + w - 1)) [y + 1 .. y + h - 2]           -- bordure en droite
  ]

--renvoie coord de la forme
coord_forme :: Forme -> Coord
coord_forme (HSegment c _) = c
coord_forme (VSegment c _) = c
coord_forme (Rectangle c _ _) = c

--tous les coords de la forme
tous_coords_forme :: Forme -> [Coord]
tous_coords_forme (HSegment (C x y) l') = let l = l' `div` caseSize in [C x' y | x' <- [x .. x + l]]
tous_coords_forme (VSegment (C x y) l') = let l = l' `div` caseSize in [C x y' | y' <- [y .. y + l]]
tous_coords_forme (Rectangle (C x y) w' h') = let w = w' `div` caseSize
                                                  h = h' `div` caseSize
                                            in  [C x' y' | x' <- [x .. x + w], y' <- [y .. y + h]]

--calculer la coordonnee de la case(colonne,row) à partir de la coordonnee de l'écran(pixel)
coordToRowCol :: Coord -> Coord
coordToRowCol (C x y) = C (x `div` caseSize) (y `div` caseSize)

--tous les cases de la forme
formeCases :: Forme -> [Coord]
formeCases forme = nub (tous_coords_forme forme) -- supprimer les doublons





                           


