module Type where

import Data.Tree (Tree(Node))
import qualified Data.Tree as Tr

-- Coord is used for both coordinate and vector
data Coord = Coord Double Double deriving Show
data Tri = TriCoord Coord Coord Coord | TriReg {origin :: Coord, side :: Double, angle :: Double}

instance Show Tri where
    show (TriReg (Coord x y) s a) = concat ["TriReg(",show x,",",show y," ",show s," ",show a,")"]

-- line connecting between triangle centers
data Line = Line Tri Tri deriving Show

(Coord x1 y1) +: (Coord x2 y2) = Coord (x1+x2) (y1+y2)
(Coord x1 y1) -: (Coord x2 y2) = Coord (x1-x2) (y1-y2)
k *: (Coord x y) = Coord (k*x) (k*y)
(Coord x y) /: k = Coord (x/k) (y/k)

normalize :: Coord -> Coord
normalize v@(Coord x y) =
    let l = norm v
    in Coord (x/l) (y/l)

norm :: Coord -> Double
norm (Coord x y) = sqrt (x*x+y*y)

-- a is in radian
rot a (Coord x y) = Coord (x*cos a+y*sin a) (-x*sin a+y*cos a)

data DrawType = DrawLine | DrawTri


parent_child_pairs :: Tree a -> [(a,a)]
parent_child_pairs (Node l cs) =
    concat (map parent_child_pairs cs) ++ map (\(Node cl _) -> (l,cl)) cs

parent_child_pairs2 :: Tree a -> [(a,a,Bool)]    -- with flag of "the leaf node?"
parent_child_pairs2 (Node l cs) =
    concat (map parent_child_pairs2 cs) ++ map (\(Node cl ccs) -> (l,cl,null ccs)) cs

toTriCoord :: Tri -> Tri
toTriCoord a@(TriCoord _ _ _) = a
toTriCoord (TriReg o s a) =
    let f b = o +: rot b (Coord (s/2 / cos (pi/6)) 0)
    in TriCoord (f (pi/2+a)) (f (pi/2+2*pi/3+a)) (f (pi/2+4*pi/3+a))
