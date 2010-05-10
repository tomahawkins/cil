module Language.CIL
  ( File (..)
  )
  where

import Text.Parse

data File = File String
  deriving Show {-!derive : Parse !-}


