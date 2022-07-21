module Main where

import Lib

main :: IO ()
main = do
    arg <- getLine
    putStr arg
