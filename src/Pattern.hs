module Pattern where

import Type 


import Data.Tree (Tree(Node))
import qualified Data.Tree as Tr
import Data.Text(Text)
import qualified Data.Text as T (concat,pack,intercalate,append)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

split_triangle :: Tri -> [Tri]
split_triangle (TriReg o s a) =
    let
        f b = TriReg (o +: rot b (Coord (s/4 / cos (pi/6)) 0)) (s/2) a
    in
    [
    TriReg o (s/2) (a+pi),
        f (pi/2+a),
        f (2*pi/3+pi/2+a),
        f (4*pi/3+pi/2+a)
        ]
split_triangle _ = error "unimplemented"

tri = TriReg (Coord 0 0) 20 0


recursive_split :: Int -> Int -> [Tri] -> [Tri]
recursive_split max_level level tri =
    if max_level < level then tri
    else
        let tris = concatMap split_triangle tri
        in recursive_split max_level (level+1) tris ++ tri


-- Note: Is it possible to make tree traversal a tail call?
recursive_split2 :: Int -> Int -> Tri -> Tree Tri
recursive_split2 max_level level tri =
    if max_level < level then Node tri []
    else
        let
            tris = split_triangle tri
            cs = map (recursive_split2 max_level (level+1)) tris
        in Node tri cs

get_obj :: Int -> (Tree Coord, [Tri])
get_obj max_level =
    let tr = recursive_split2 max_level 1 tri
        lines = connect tr
        ts = Tr.flatten tr
    in (lines,ts)


connect :: Tree Tri -> Tree Coord
connect t = remove_duplicate $ fmap origin t 


remove_duplicate :: Tree Coord -> Tree Coord
remove_duplicate t =
    let temp = add_history t
    in temp

tree_line_example :: Tree Coord
tree_line_example = Node (Coord 50 50) [n 0 100,n 100 0,n 100 100]
    where n x y = Node (Coord x y) []


-- add_history :: Tree Coord -> Tree (Coord,[])
add_history = id


mk_tri :: Double -> Bool -> Coord -> Coord -> [Coord]
mk_tri angle corrected p@(Coord px py) q@(Coord qx qy) =
    let
        dir = rot (pi/2) $ normalize (q -: p)
        l = norm (q -: p) * tan (angle / 2)
        lv = l *: dir
    in
        if corrected then
            [p +: (2*:lv), p -: (2*:lv), q-:lv,q+:lv]
        else
            [p +: lv, p -: lv, (Coord qx qy)]
