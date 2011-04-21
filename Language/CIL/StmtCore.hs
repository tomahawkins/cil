module Language.CIL.StmtCore
  ( Stmt  (..)
  ) where

import Language.C hiding (Name)

import Language.CIL.Types

-- | Statements.
data Stmt
  = Null
  | Label Name Stmt Position
  | Compound [Stmt] Position
  | TypeDecl Name Type Position
  | VariableDef Name Type (Maybe Init) Position
  | FunctionDef Name Type [(Name, Type)] Stmt Position
  | AssignApply Expr Apply Position
  | AssignExpr Expr Expr Position
  | StmtApply Apply Position
  | While Expr Stmt Position
  | If Expr Stmt Stmt Position
  | Return (Maybe Expr) Position
  | Goto Name Position
  | Break Position
  | Switch Expr Stmt Position
  | Case Expr Stmt Position
  | Default Stmt Position
  deriving (Show, Eq)

instance Pos Stmt where
  posOf a = case a of
    Null -> undefined
    Label _ _ p -> p
    Compound _ p -> p
    TypeDecl _ _ p -> p
    VariableDef _ _ _ p -> p
    FunctionDef _ _ _ _ p -> p
    AssignExpr _ _ p -> p
    AssignApply _ _ p -> p
    StmtApply _ p -> p
    While _ _ p -> p
    If _ _ _ p -> p
    Return _ p -> p
    Goto _ p -> p
    Break p -> p
    Switch _ _ p -> p
    Case _ _ p -> p
    Default _ p -> p

