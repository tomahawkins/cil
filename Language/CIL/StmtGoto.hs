module Language.CIL.StmtGoto
  ( Stmt  (..)
  ) where

import Language.C hiding (Name)

import Language.CIL.Types

-- | Statements.
data Stmt
  = Null
  | Compound [Name] [Stmt] Position
  | TypeDecl Name Type Position
  | VariableDef Name Type (Maybe Init) Position
  | AssignExpr Expr Expr Position
  | If Expr Stmt Stmt Position
  | Goto Name Position
  deriving (Show, Eq)

instance Pos Stmt where
  posOf a = case a of
    Null -> undefined
    Compound _ _ p -> p
    TypeDecl _ _ p -> p
    VariableDef _ _ _ p -> p
    AssignExpr _ _ p -> p
    If _ _ _ p -> p
    Goto _ p -> p
