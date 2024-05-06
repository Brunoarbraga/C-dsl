module Main where

import DSL
import FileGen
import Hlist

main :: IO ()
main = do
  writeCFile "fact.c" factorial
  writeCFile "sum.c" sumInput
  print example3
