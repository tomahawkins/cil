module Language.CIL.Utils
  ( err
  , err'
  , notSupported
  , notSupported'
  , position
  ) where

import Language.C

err :: (Pretty a, Pos a) => a -> String -> b
err a m = error $ position a ++ ": " ++ m ++ ": " ++ show (pretty a)

err' :: Pos a => a -> String -> b
err' a m = error $ position a ++ ": " ++ m

notSupported :: (Pretty a, Pos a) => a -> String -> b
notSupported a m = err a $ "not supported: " ++ m

notSupported' :: Pos a => a -> String -> b
notSupported' a m = err' a $ "not supported: " ++ m

-- | Format the file position of something with ties to the orignial source, like a 'Stmt' or 'Expr'.
position :: Pos a => a -> String
position a = f ++ ":" ++ show l ++ ":" ++ show c
  where
  (f,l,c) = (posFile p, posRow p, posColumn p)
  p = posOf a

