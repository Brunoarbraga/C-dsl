module Main where

import Quickcheck
import Test.QuickCheck 



main :: IO ()
main = do
  iphdr <- generate (arbitrary :: Gen Tcphdr)
  print iphdr
  --writeCFile "fact.c" factorial
  --writeCFile "sum.c" sumInput
  return ()
  
