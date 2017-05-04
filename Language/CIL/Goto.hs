module Language.CIL.Goto (goto) where

import Control.Monad.State

import Language.CIL.StmtCore
import qualified Language.CIL.StmtGoto as G
import Language.CIL.Types
import Language.CIL.Utils

-- | Given a max call depth and a 'Stmt' program, reduces a program to assignments, if statements, and goto jumps.
--   The function errors out if during function inlining process, the call depth exceeds the max depth.
goto :: Int -> Stmt -> G.Stmt
goto maxDepth stmt = convert $ evalState (reduce maxDepth [] stmt) (0, [])

type Label = State (Int, [(Name, Name)])

nextLabel :: Label Name
nextLabel = do
  (n, t) <- get
  put (n + 1, t)
  return $ "__label_" ++ show n

label :: Name -> Label Name
label name = do
  (_, t) <- get
  case lookup name t of
    Nothing -> do
      l <- nextLabel
      (n, t) <- get
      put (n, t ++ [(name, l)])
      return l
    Just l -> return l

withScope :: Label a -> Label a
withScope scope = do
  (n, t) <- get
  put (n, [])
  a <- scope
  (n, _) <- get
  put (n, t)
  return a

convert :: Stmt -> G.Stmt
convert a = case a of
  Null -> G.Null
  Label name stmt p -> G.Label name (convert stmt) p
  Compound stmts p -> G.Compound (map convert stmts) p
  TypeDecl name typ p -> G.TypeDecl name typ p
  VariableDef name typ init p -> G.VariableDef name typ init p
  AssignExpr a b p -> G.AssignExpr a b p
  If a b c p -> G.If a (convert b) (convert c) p
  Goto name p -> G.Goto name p
  a -> err' a $ "goto reduction failed: " ++ show a

reduce :: Int -> [Name] -> Stmt -> Label Stmt
reduce maxDepth callTrace a = case a of
  Null -> return Null
  TypeDecl _ _ _ -> return a
  VariableDef _ _ _ _ -> return a
  AssignExpr _ _ _ -> return a
  Goto name p -> do
    name <- label name
    return $ Goto name p
  Compound stmts p -> do
    stmts <- mapM (reduce maxDepth callTrace) stmts
    return $ Compound stmts p
  If a b c p -> do
    b <- reduce maxDepth callTrace b
    c <- reduce maxDepth callTrace c
    return $ If a b c p
  While cond stmt p -> do
    before <- nextLabel
    after  <- nextLabel
    stmt <- reduceWhile maxDepth callTrace after stmt
    return $ Label before (Compound [If cond (Compound [stmt, Goto before p] p) Null p, Label after Null p] p) p
  Break _ -> notSupported' a "break outside of loop or switch"
  Case _ _ _ -> notSupported' a "case outside of switch"
  Default _ _ -> notSupported' a "default outside of switch"
  Switch cond stmt p -> notSupported' a "reduce of Switch is not supported."
  _ -> notSupported' a "statement for goto transform"

  {-
  FunctionDef Name Type [(Name, Type)] Stmt Position
  AssignApply Expr Apply Position
  StmtApply Apply Position
  Return (Maybe Expr) Position
  -}

reduceWhile :: Int -> [Name] -> Name -> Stmt -> Label Stmt
reduceWhile maxDepth callTrace after a = case a of
  Break p -> return $ Goto after p
  Compound stmts p -> do
    stmts <- mapM (reduceWhile maxDepth callTrace after) stmts
    return $ Compound stmts p
  If a b c p -> do
    b <- reduceWhile maxDepth callTrace after b
    c <- reduceWhile maxDepth callTrace after c
    return $ If a b c p
  a -> reduce maxDepth callTrace a
