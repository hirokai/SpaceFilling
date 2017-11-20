module Metric where

import Type

x (Coord _x _) = _x
y (Coord _ _y) = _y

distFromLineSegment :: Coord -> Coord -> Coord -> Double
distFromLineSegment (Coord x3 y3) (Coord x1 y1) (Coord x2 y2) =
    let
        px = x2 - x1
        py = y2 - y1
        something = px*px + py*py
        u = ((x3-x1)*px+(y3-y1)*py)/something
        u2 = if u > 1 then 1 else (if u < 0 then 0 else u)
        x = x1 + u2 * px
        y = y1 + u2 * py
    in
        norm (Coord (x-x3) (y-y3))

distFromTriangle :: Double -> Coord -> [Coord] -> Double
distFromTriangle threshold p qs =
    let
        xs = map x qs
        ys = map y qs
    in
    -- in if minimum xs - x p > threshold || x p - maximum xs > threshold ||
    --     minimum ys - y p > threshold || y p - maximum ys > threshold
    if inBound p qs
    then 0
    else
        case qs of
            [a,b,c] -> minimum [distFromLineSegment p a b, distFromLineSegment p b c, distFromLineSegment p c a]
            [a,b,c,d] -> minimum [distFromLineSegment p a b, distFromLineSegment p b c, distFromLineSegment p c d, distFromLineSegment p d a]

inTriangle p qs = True

-- https://stackoverflow.com/a/2922778
inBound :: Coord -> [Coord] -> Bool
inBound p qs =
    let
        combinations = case qs of
                            [a,b,c] -> [(a,b),(b,c),(c,a)]
                            [a,b,c,d] -> [(a,b),(b,c),(c,d),(d,a)]
        f (a,b) = (y a > y p) /= (y b > y p) && x p < (x b - x a) * (y p - y a) / (y b - y a) + x a
    in
        odd . sum $ map ((\b -> if b then 1 else 0) . f) combinations

inBoundTri :: Double -> Coord -> Bool
inBoundTri side p =
    let
        TriCoord a b c = toTriCoord $ TriReg (Coord 0 0) side 0
    in
        inBound p [a,b,c]

mk_csv_line threshold width resolution tris xi yi =
    let
        f i = (fromIntegral i / fromIntegral resolution - 0.5) * width
        p@(Coord x y) = Coord (f xi) (f yi)
        v = if inBoundTri 20 p then minimum $ map (distFromTriangle threshold p) tris else -1
    in
        show xi ++ "," ++ show yi ++ "," ++ show x ++ "," ++ show y ++ "," ++ (show v) ++ "\n"

write_distance_map path threshold resolution tris = do
    let width = 30
    let idxs = [(xi,yi)| xi <- [0..resolution], yi <- [0..resolution]]
    let csv_str = concatMap (uncurry (mk_csv_line threshold width resolution tris)) idxs
    writeFile path csv_str
