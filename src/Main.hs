{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T (concat,pack)
import Data.Tree (Tree(Node))
import qualified Data.Tree as Tr
import Control.Monad (forM_)

import Safe (headMay)
import System.IO.Unsafe (unsafePerformIO)

import Type
import Svg (write_svg)
import Pattern
import Dxf (write_dxf)
   
main :: IO ()
main = do
    -- forM_ [1..8] (run_dxf 1)
    -- forM_ [1..8] (run_dxf 3)
    -- forM_ [1..8] (run_dxf 5)
    -- forM_ [1..10] (run_dxf 5)
    -- forM_ [6] (run_dxf 1)
    -- forM_ [1..8] (run_dxf 7)
    run 6
    -- forM_ [1..8] (run_dxf 10)


run :: Int -> IO ()
run max_level = do
    let tr = recursive_split2 max_level 1 tri
    let lines = connect tr
    let ts = Tr.flatten tr
    -- putStrLn $ Tr.drawTree (fmap show tr)
    -- TIO.putStrLn (tshow (map toTriCoord ts))
    write_svg lines ts

run_dxf :: Double -> Int -> IO ()
run_dxf angle max_level = do
    let tr = recursive_split2 max_level 1 tri
    let lines = connect tr
    let ts = Tr.flatten tr
    let path = "trees " ++ show max_level ++ "G " ++ show angle ++ " deg.dxf"
    putStrLn $ "Generating...: " ++ path
    write_dxf path angle lines ts
