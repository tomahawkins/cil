module Main where

import Language.CIL

main :: IO ()
main = do
  installPlugin
  f <- parseC "test.c"
  print f

