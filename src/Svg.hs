{-# LANGUAGE OverloadedStrings #-}

module Svg where

import Data.Tree (Tree(Node))
import qualified Data.Tree as Tr

import Data.Text (Text)
import qualified Data.Text as T (concat,pack)
import Data.Text.Lazy (toStrict)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import Control.Monad (forM_)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as SA
import Text.Blaze.Svg.Renderer.String (renderSvg)

import Safe (headMay)

import Type
import Pattern
import Metric

to_svgPath :: [Tri] -> S.Svg
to_svgPath ts =
    S.path ! SA.strokeWidth "1" ! SA.stroke "#ccc" ! SA.fill "none" !
        SA.d (H.toValue $ T.concat (map drawTri ts))

drawTri :: Tri -> Text
drawTri (TriCoord (Coord x1 y1) (Coord x2 y2) (Coord x3 y3)) =
    T.concat ["M ", tshow x1, " ", tshow y1," L ", tshow x2, " ", tshow y2," L ", tshow x3, " ", tshow y3, " L ", tshow x1, " ", tshow y1]
drawTri t =
    drawTri (toTriCoord t)


mkPath :: Int -> Text -> Text -> [Coord] -> Text
mkPath w s f ps =
    let Coord x0 y0 = head ps
    in 
    T.concat ["<path stroke-width='",tshow w,"' stroke='",tshow s,"' fill='",tshow f,"' d='M ", tshow x0, " ", tshow y0,
        T.concat (map (\(Coord x y) -> T.concat [" L ", tshow x, " ", tshow y]) ps),
        "'></path>"]

write_svg path angle lines ts = do
    let tris = mkTriangles angle lines
    let path_map = path ++ ".distance.csv"
    write_distance_map path_map 10 1200 tris
    TLIO.writeFile path $ renderHtml $
        H.html $ H.body $
            S.svg ! SA.width "1200" ! SA.height "1200" $ do
                S.g ! SA.transform "translate (100,100) scale (5)" $ do
                    S.g $ do
                        mapM_ (\t -> to_svgPath [t]) ts
                    trianglesToSvg tris

mk_path_d :: Bool -> [Coord] -> Text
mk_path_d closed ps =
    case headMay ps of
        Just (Coord p0x p0y) ->
            T.concat [
                T.concat ["M ", tshow p0x, " ", tshow p0y],
                T.concat (map (\(Coord x y) -> T.concat [" L ", tshow x, " ", tshow y]) (tail ps)),
                if closed then " z" else ""
            ]
        Nothing -> ""


tree_line_to_svg :: DrawType -> Tree Coord -> Double -> S.Svg
tree_line_to_svg typ t angle = 
    let ps = remove_invalid $ parent_child_pairs2 t
    in
        S.g $ do
            forM_ ps $ \(p,q,leaf) -> do
                case typ of
                    DrawLine ->
                        S.path ! SA.strokeWidth "1" ! SA.stroke "red" ! SA.fill "none" !
                            SA.d (S.toValue $ mk_path_d False [p,q])
                    DrawTri ->
                        S.path ! SA.strokeWidth "0.2" ! SA.stroke "none" ! SA.fill "blue" !
                            SA.d (S.toValue $ mk_path_d True (mk_tri (angle*pi/180) (not leaf) p q))


mkTriangles :: Double -> Tree Coord -> [[Coord]]
mkTriangles angle t =
    let ps = remove_invalid $ parent_child_pairs2 t
    in map (\(p,q,leaf) -> mk_tri (angle*pi/180) (not leaf) p q) ps

-- Draw filled triangles (space-filling trees with tapered angle)
trianglesToSvg :: [[Coord]] -> S.Svg
trianglesToSvg tris =
    S.g $ do
        forM_ tris $ \tri ->
            S.path ! SA.strokeWidth "0.2" ! SA.stroke "none" ! SA.fill "blue" !
                SA.d (S.toValue $ mk_path_d True tri)


draw_level level angle = do
    let (lines,ts) = get_obj level
    S.svg ! SA.width "700" ! SA.height "500" $ do
        S.g ! SA.transform "translate (100,100) scale (5)" $ do
            S.g $ do
                mapM_ (\t -> to_svgPath [t]) ts
            tree_line_to_svg DrawTri lines angle

run_multi :: IO ()
run_multi = do
    TLIO.writeFile "multi.html" $ renderHtml $
        H.html $ H.body $ do
            H.div $ do
                draw_level 2 5
            H.div $ do
                draw_level 4 5
            H.div $ do
                draw_level 6 5
