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
    -- forM_ [1..8] (run_dxf 10)
    -- forM_  [(x,y) | y <- [1..9], x <- [1,5,10]] (uncurry run_svg)
    -- mapM_ (run_svg 5) [1,6,7,8,9]
    mapM_ (run_svg 1) [2,3,4]


run_svg :: Double -> Int -> IO ()
run_svg angle max_level = do
    let path = "trees " ++ show max_level ++ "G " ++ show angle ++ " deg.html"
    putStrLn $ "Generating...: " ++ path
    let tr = recursive_split2 max_level 1 tri
    let lines = connect tr
    let ts = Tr.flatten tr
    write_svg path angle lines ts

run_dxf :: Double -> Int -> IO ()
run_dxf angle max_level = do
    let tr = recursive_split2 max_level 1 tri
    let lines = connect tr
    let ts = Tr.flatten tr
    let path = "trees " ++ show max_level ++ "G " ++ show angle ++ " deg.dxf"
    putStrLn $ "Generating...: " ++ path
    write_dxf path angle lines ts
