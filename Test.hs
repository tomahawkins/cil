module Main where

import Language.CIL

main :: IO ()
main = do
  f <- debugParseC "test.c"
  print f

