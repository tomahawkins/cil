module Main where

import Language.CIL
import Text.Parse

main :: IO ()
main = print $ (runParser parse "File \"hi\"" :: (Either String File, String))

