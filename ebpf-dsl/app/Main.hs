module Main where

import DSL
import FileGen
import Hlist
import Hlist (hLookup)

main :: IO ()
main = do
  --writeCFile "fact.c" factorial
  --writeCFile "sum.c" sumInput
  hLookup 
  print example3
