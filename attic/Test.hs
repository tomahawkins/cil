module Main where

import Language.CIL

--import Log

main :: IO ()
main = do
  --debugParseC "test.c"
  --print f
  parseC "test.c" >>= print

