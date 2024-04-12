module Formes where

data Coord = C {cx :: Int, cy :: Int} deriving (Show , Eq)

data Forme = HSegment Coord Int
           | VSegment Coord Int
           | Rectangle Coord Int Int
-- Hsegment : Coord : point de le plus à l’Ouest
-- Vsegment : Coord : point de le plus au Nord
-- Rectangle : Coord : point de le plus au Nord-Ouest

-- y:Nord -> y:Sud -> x:Ouest -> x:Est
limites :: Forme -> (Int, Int, Int, Int)
limites (HSegment (C x y) l) = (y, y, x, x + l)
limites (VSegment (C x y) l) = (y, y + l, x, x)
limites (Rectangle (C x y) l h) = (y, y + h, x, x + l)


--si la case aux coordonnees donnes appartient à la forme
appartient :: Coord -> Forme -> Bool
appartient (C x y) (HSegment (C x' y') l) = y == y' && x >= x' && x < x' + l
appartient (C x y) (VSegment (C x' y') l) = x == x' && y >= y' && y < y' + l
appartient (C x y) (Rectangle (C x' y') l h) = x >= x' && x < x' + l && y >= y' && y < y' + h


-- true si collision,mais possible : faux positive
collisionApprox :: Forme -> Forme -> Bool
collisionApprox f1 f2 = let (y1, y2, x1, x2) = limites f1
                            (y1', y2', x1', x2') = limites f2
                        in (x1 <= x2' && x2 >= x1' && y1 <= y2' && y2 >= y1')

-- une fonction adjacent :: Coord -> Forme -> Bool qui
-- prend en entr´ee des coordonn´ees et une forme, et d´ecide si la case aux coordonn´ees donn´es est contig¨ue `a
-- (c’est-`a-dire juste `a cˆot´e de) la forme (on ne demande pas son code dans cette ´epreuve).

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



                           


