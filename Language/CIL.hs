module Language.CIL
  ( Stmt (..)
  , parseCIL
  ) where

import Data.ByteString (ByteString)
import Language.C
import Language.C.Data.Ident

data Stmt
  = Stmt
  deriving (Show, Eq)

-- | Parses a CIL program, given a file name and contents.
parseCIL :: String -> ByteString -> Stmt
parseCIL name code = case parseC code (initPos name) of
    Left e  -> error $ "parsing error: " ++ show e
    Right a -> convert $ rewrite a

-- | Rewrites a program to a single statement.
rewrite :: CTranslUnit -> CStat
rewrite (CTranslUnit items _) = CCompound [] (map f items ++ [CBlockStmt callMain]) none
  where
  f (CDeclExt a) = CBlockDecl a
  f (CFDefExt a) = CNestedFunDef a
  f a@(CAsmExt _) = notSupported a "inline assembly"
  callMain :: CStat
  callMain = CExpr (Just $ CCall (CVar (Ident "main" 0 none) none) [] none) none
  none :: NodeInfo
  none = internalNode

-- | Converts 'CStat' to 'Stmt'.
convert :: CStat -> Stmt
convert _ = Stmt








err :: (Pretty a, Pos a) => a -> String -> b
err a m = error $ position a ++ ": " ++ m ++ ": " ++ show (pretty a)

notSupported :: (Pretty a, Pos a) => a -> String -> b
notSupported a m = err a $ "not supported: " ++ m

unexpected :: (Pretty a, Pos a) => a -> String -> b
unexpected a m = err a $ "unexpected: " ++ m

err' :: Pos a => a -> String -> b
err' a m = error $ position a ++ ": " ++ m

notSupported' :: Pos a => a -> String -> b
notSupported' a m = err' a $ "not supported: " ++ m

unexpected' :: Pos a => a -> String -> b
unexpected' a m = err' a $ "unexpected: " ++ m

position :: Pos a => a -> String
position a = f ++ ":" ++ show l ++ ":" ++ show c
  where
  Position f l c = posOf a

