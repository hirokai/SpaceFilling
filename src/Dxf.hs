{-# LANGUAGE OverloadedStrings #-}

module Dxf where

import Data.Tree (Tree(Node))
import qualified Data.Tree as Tr

import Data.Text (Text)
import qualified Data.Text as T (concat,pack,intercalate,append)
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
import System.IO.Streams


polyline_header s = do
    write (Just "  0\nPOLYLINE\n  8\nLAYER_0\n 66\n   1\n 70\n   1\n 40\n0\n 41\n0\n") s

polyline_footer s = do
    write (Just "  0\nSEQEND\n") s

x (Coord _x _) = _x
y (Coord _ _y) = _y

contents :: OutputStream Text -> Double -> Tree Coord -> IO ()
contents s angle lines = do
    let ls = remove_invalid $ parent_child_pairs2 lines
    forM_ ls $ \(p,q,leaf) -> do
        let ps = mk_tri (angle*pi/180) (not leaf) p q
        polyline_header s
        forM_ ps $ \p ->
            write (Just $ T.concat ["  0\nVERTEX\n  8\nLAYER_0\n 10\n",tshow (x p), "\n 20\n", tshow (y p), "\n"]) s
        polyline_footer s

write_dxf path angle lines ts = do
    let header = "0\nSECTION\n  2\nHEADER\n  0\nENDSEC\n  0\nSECTION\n  2\nTABLES\n  0\nTABLE\n  2\nLAYER\n 70\n   2\n  0\nLAYER\n  2\n0\n 70\n    0\n 62\n    2\n  6\nCONTINUOUS\n  0\nLAYER\n  2\nLAYER_0\n 70\n    0\n 62\n    2\n  6\nCONTINUOUS\n  0\nENDTAB\n  0\nENDSEC\n  0\nSECTION\n  2\nBLOCKS\n  0\nENDSEC\n  0\nSECTION\n  2\nENTITIES\n"
    let footer = "    0\nENDSEC\n  0\nEOF\n"
    withFileAsOutput path $ \s1 -> do
        s <- encodeUtf8 s1
        write (Just header) s
        contents s angle lines
        write (Just footer) s
    -- TIO.writeFile "testout.dxf" entities_txt