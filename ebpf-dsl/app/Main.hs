module Main where

import DSL
import FileGen

main :: IO ()
main = do
  writeCFile "fact.c" factorial
  writeCFile "sum.c" sumInput
