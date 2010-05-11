module Main where

import Language.CIL

main :: IO ()
main = do
  f <- parseC "test.c"
  print f

