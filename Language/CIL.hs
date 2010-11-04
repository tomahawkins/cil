module Language.CIL
  ( Stmt (..)
  , Type (..)
  , parseCIL
  ) where

import Data.ByteString (ByteString)
import Language.C hiding (Name)
import Language.C.Data.Ident

type Name = String

data Type
  = Void
  | Array Int Type
  | Ptr Type
  | Volatile Type
  | Struct [(Name, Type)]
  | Union  [(Name, Type)]
  | StructRef  Name
  | UnionRef   Name
  | Int8
  | Int16
  | Int32
  | Word8
  | Word16
  | Word32
  | Float
  | Double
  deriving (Show, Eq)

data Stmt
  = Compound [Name] [Stmt] Position
  | TypeDecl Name Type Position
  deriving (Show, Eq)

instance Pos Stmt where
  posOf a = case a of
    Compound _ _ p -> p
    TypeDecl _ _ p -> p

-- | Parses a CIL program, given a file name and contents.
parseCIL :: String -> ByteString -> Stmt
parseCIL name code = case parseC code (initPos name) of
    Left e  -> error $ "parsing error: " ++ show e
    Right a -> convertStat $ rewrite a

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

-- | Name of identifier.
name :: Ident -> Name
name (Ident name _ _) = name

-- | Converts 'CStat' to 'Stmt'.
convertStat :: CStat -> Stmt
convertStat a = case a of
  CLabel i a [] _ -> Compound [name i] [convertStat a] p
  CCompound ids items _ -> Compound (map name ids) (map convertBlockItem items) p
  {-
  CIf a b Nothing n -> evalStat env $ CIf a b (Just $ CCompound [] [] n) n
  CIf a b (Just c) n -> do
    a <- latchBool (posOf n) $ evalExpr env a
    branch (posOf n) a (evalStat env b) (evalStat env c)
    return ()

  CExpr Nothing _ -> return ()
  CExpr (Just (CAssign op a b n)) _ -> case op of
    CAssignOp -> case evalExpr env a of
      Var v -> assign (posOf n) v $ evalExpr env b
      _ -> unexpected a "non variable in left hand of assignment"
    CMulAssOp -> f CMulOp
    CDivAssOp -> f CDivOp
    CRmdAssOp -> f CRmdOp
    CAddAssOp -> f CAddOp
    CSubAssOp -> f CSubOp
    CShlAssOp -> f CShlOp
    CShrAssOp -> f CShrOp
    CAndAssOp -> f CAndOp
    CXorAssOp -> f CXorOp
    COrAssOp  -> f COrOp
    where
    f :: CBinaryOp -> M ()
    f op = evalStat env (CExpr (Just (CAssign CAssignOp a (CBinary op a b n) n)) n)

  CExpr (Just (CCall (CVar f _) args _)) _ -> apply env' func' (map (evalExpr env) args) >> return ()
    where
    (env', func') = case env f of
      Function env func -> (env, func)
      _ -> unexpected' f "environment returned something other than a function"

  CExpr (Just (CCall _ _ _)) _ -> notSupported a "non named function references"

  CExpr (Just (CUnary op a n1)) n2 | elem op [CPreIncOp, CPostIncOp] -> evalStat env (CExpr (Just (CAssign CAddAssOp a one n1)) n2)
                                   | elem op [CPreDecOp, CPostDecOp] -> evalStat env (CExpr (Just (CAssign CSubAssOp a one n1)) n2)
    where
    one = CConst $ CIntConst (cInteger 1) n1

  -}
  _ -> notSupported a "statement"
  where
  p = posOf a

convertBlockItem :: CBlockItem -> Stmt
convertBlockItem a = case a of
  CBlockStmt    a -> convertStat   a
  CBlockDecl    a -> convertDecl   a
  CNestedFunDef a -> convertFunDef a

convertDecl :: CDecl -> Stmt
convertDecl a = case a of
  CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just (Ident name _ _)) (Just decls) [] _) _)] [] _ -> TypeDecl name (Struct $ map field decls) p
  CDecl [CTypeSpec (CSUType (CStruct CUnionTag  (Just (Ident name _ _)) (Just decls) [] _) _)] [] _ -> TypeDecl name (Union  $ map field decls) p
  _ -> notSupported a "declaration"
  where
  p = posOf a

-- | Struct and union fields.
field :: CDecl -> (Name, Type)
field a = case a of
  CDecl t [(Just (CDeclr (Just (Ident name _ _)) derived Nothing [] _), Nothing, Nothing)] _ -> (name, typ t derived)
  _ -> notSupported a "struct/union field"

typ :: [CDeclSpec] -> [CDerivedDeclr] -> Type
typ a b = derivedDeclrs (cDeclSpec a) b

cDeclSpec :: [CDeclSpec] -> Type
cDeclSpec a = case a of
  [] -> Void
  [CTypeSpec (CSUType (CStruct CStructTag (Just (Ident name _ _)) Nothing [] _) _)] -> StructRef name
  [CTypeSpec (CSUType (CStruct CUnionTag  (Just (Ident name _ _)) Nothing [] _) _)] -> UnionRef  name
  CTypeQual (CVolatQual _) : a -> Volatile $ cDeclSpec a
  CStorageSpec _ : a -> cDeclSpec a  -- Ignore storage specs.
  CTypeSpec (CSignedType _) : a -> cDeclSpec a
  CTypeSpec (CUnsigType _)  : a -> case cDeclSpec a of
    Int8  -> Word8
    Int16 -> Word16
    Int32 -> Word32
    _ -> notSupported' a "type specification"
  CTypeSpec (CCharType   _) : _ -> Int8
  CTypeSpec (CShortType  _) : _ -> Int16
  CTypeSpec (CLongType   _) : _ -> Int32
  CTypeSpec (CIntType    _) : _ -> Int32
  CTypeSpec (CFloatType  _) : _ -> Float
  CTypeSpec (CDoubleType _) : _ -> Double
  _ -> notSupported' a "type specification"

derivedDeclrs :: Type -> [CDerivedDeclr] -> Type
derivedDeclrs t a = foldl derivedDeclr t a

derivedDeclr :: Type -> CDerivedDeclr -> Type
derivedDeclr t a = case a of
  CPtrDeclr quals _ -> volatile quals $ Ptr t
  CArrDeclr quals (CArrSize _ (CConst (CIntConst i _))) _ -> volatile quals $ Array (fromIntegral $ getCInteger i) t
  _ -> notSupported' a "derived declaration"
  where
  volatile quals typ = if any isVolatile quals then Volatile typ else typ
  isVolatile (CVolatQual _) = True
  isVolatile _              = False

convertFunDef :: CFunDef -> Stmt
convertFunDef a = notSupported a "function"


{-
-- No stateful operations for expressions.
evalExpr :: Env -> CExpr -> E
evalExpr env a = case a of
  CCond a (Just b) c n -> Mux (evalExpr env a) (evalExpr env b) (evalExpr env c) $ posOf n
  CCond a Nothing  b n -> Or (evalExpr env a) (evalExpr env b) $ posOf n
  CBinary op a' b' n -> case op of
    CMulOp -> Mul a b p
    CDivOp -> Div a b p
    CRmdOp -> Mod a b p
    CAddOp -> Add a b p
    CSubOp -> Sub a b p
    CShlOp -> notSupported' a "(<<)"
    CShrOp -> notSupported' a "(>>)"
    CLeOp  -> Lt a b p
    CGrOp  -> Lt b a p
    CLeqOp -> Not (Lt b a p) p
    CGeqOp -> Not (Lt a b p) p
    CEqOp  -> Eq a b p
    CNeqOp -> Not (Eq a b p) p
    CAndOp -> notSupported' a "(&)"
    CXorOp -> notSupported' a "(^)"
    COrOp  -> notSupported' a "(|)"
    CLndOp -> And a b p
    CLorOp -> Or a b p
    where
    a = evalExpr env a'
    b = evalExpr env b'
    p = posOf n

  CUnary op a' n -> case op of
    CPlusOp -> a
    CMinOp  -> Sub zero a p
    CNegOp  -> Not a p
    --(CAdrOp,  a) -> return $ Ref a p
    --(CIndOp,  a) -> return $ Deref a p
    _ -> notSupported' n "unary operator"
    where
    a = evalExpr env a'
    p = posOf n
    zero = Const $ M.CInteger 0 p

  CVar i _ -> case env i of
    Variable v -> Var v
    _ -> unexpected' i "environment returned non variable"

  CConst a -> case a of
    CIntConst (CInteger a _ _) n -> Const $ M.CInteger a $ posOf n
    CFloatConst (CFloat a) n -> Const $ CRational (toRational (read a :: Double)) $ posOf n
    _ -> notSupported a "char or string constant"
  _ -> notSupported a "expression"
    
evalDecl :: Env -> CDecl -> M Env
evalDecl env d@(CDecl specs decls _) = if isExtern typInfo then return env else foldM evalDecl' env decls
  where
  (typInfo, typ) = typeInfo specs
  evalDecl' :: Env -> (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> M Env
  evalDecl' env (a, b, c) = case a of
    Just (CDeclr (Just i@(Ident name _ n)) [] Nothing [] _) -> case (b, c) of
      (Nothing, Nothing) -> evalDecl' env (a, Just $ CInitExpr (CConst (CIntConst (cInteger 0) n)) n, Nothing)
      (Just (CInitExpr (CConst const) n'), Nothing) | isStatic typInfo && not (isVolatile typInfo) -> return $ addVar v env
        where
        v = State $ VS name typ init $ posOf n
        init = case typ of
          Void -> unexpected d "void type for variable declaration"
          Ptr _ -> notSupported d "pointer types"
          Bool -> CBool (cInt /= 0) $ posOf n'
          Integer _  -> M.CInteger cInt $ posOf n'
          Rational _ -> CRational cRat $ posOf n'

        cInt :: Integer
        cInt = case const of
          CIntConst (CInteger a _ _) _ -> a
          _ -> unexpected const "non integer initialization"

        cRat :: Rational
        cRat = case const of
          CIntConst (CInteger a _ _) _ -> fromIntegral a
          CFloatConst (CFloat a) _     -> fromIntegral (read a :: Integer)
          _ -> unexpected const "non numeric initialization"

      (Just (CInitExpr c _), Nothing) -> evalDecl' env (a, Nothing, Just c)

      (Nothing, Just e) -> do
        v <- if isVolatile typInfo
          then return $ Volatile name typ $ posOf n
          else do
            i <- nextId
            return $ Local name typ i p
        declare p v $ evalExpr env e
        return $ addVar v env
        where
        p = posOf e
      _ -> notSupported' i "variable declaration"
    
    -- Arrays.
    Just (CDeclr (Just ident) (CArrDeclr _ (CArrSize _ (CConst (CIntConst size _))) _ : _) _ _ _) -> return env --XXX

    -- Ignore function prototypes.
    Just (CDeclr _ (CFunDeclr _ _ _ :_) _ _ _) -> return env

    _ -> notSupported' d "arrays, pointers, or functional pointers (So what good is this tool anyway?)"

-}

{-
-- | Extract relavent info from a function declaration.
functionInfo :: CFunDef -> ([CDeclSpec], Ident, [CDecl], CStat)
functionInfo (CFunDef specs (CDeclr (Just ident) [(CFunDeclr (Right (args, False)) _ _)] Nothing [] _) [] stmt _) = (specs, ident, args, stmt)  --XXX What is the False?
functionInfo f = notSupported f "function"
-}


err :: (Pretty a, Pos a) => a -> String -> b
err a m = error $ position a ++ ": " ++ m ++ ": " ++ show (pretty a)

err' :: Pos a => a -> String -> b
err' a m = error $ position a ++ ": " ++ m

notSupported :: (Pretty a, Pos a) => a -> String -> b
notSupported a m = err a $ "not supported: " ++ m

notSupported' :: Pos a => a -> String -> b
notSupported' a m = err' a $ "not supported: " ++ m

position :: Pos a => a -> String
position a = f ++ ":" ++ show l ++ ":" ++ show c
  where
  Position f l c = posOf a

