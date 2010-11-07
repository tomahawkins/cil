module Language.CIL
  ( Name
  , Type (..)
  , Stmt (..)
  , Expr (..)
  , parseCIL
  , position
  ) where

import Data.ByteString (ByteString)
import Data.List
import Language.C hiding (Name)
import Language.C.Data.Ident

-- | Identifiers.
type Name = String

-- | Types.
data Type
  = Void
  | Array Int Type
  | Ptr Type
  | Volatile Type
  | Typedef Type
  | Struct [(Name, Type)]
  | Union  [(Name, Type)]
  | Enum   [(Name, Int)]
  | BitField Type [(Name, Int)]
  | StructRef  Name
  | UnionRef   Name
  | EnumRef    Name
  | TypedefRef Name
  | Function Type [Type]
  | Int8
  | Int16
  | Int32
  | Word8
  | Word16
  | Word32
  | Float
  | Double
  deriving (Show, Eq)

-- | Statements.
data Stmt
  = Null
  | Compound [Name] [Stmt] Position
  | TypeDecl Name Type Position
  | VariableDef Name Type (Maybe ()) Position
  | FunctionDef Name Type [(Name, Type)] Stmt Position
  | Assign Expr Expr Position
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

-- | Expressions.
data Expr
  = ConstInt    Int    Position
  | ConstFloat  Double Position
  | ConstChar   Char   Position
  | ConstString String Position
  | Var    Name      Position
  | Mul    Expr Expr Position 
  | Div    Expr Expr Position 
  | Rmd    Expr Expr Position 
  | Add    Expr Expr Position 
  | Sub    Expr Expr Position 
  | Shl    Expr Expr Position 
  | Shr    Expr Expr Position 
  | Lt     Expr Expr Position 
  | Gt     Expr Expr Position 
  | Le     Expr Expr Position 
  | Ge     Expr Expr Position 
  | Eq     Expr Expr Position 
  | Neq    Expr Expr Position 
  | And    Expr Expr Position 
  | Xor    Expr Expr Position 
  | Or     Expr Expr Position 
  | Adr    Expr      Position
  | Ind    Expr      Position
  | Minus  Expr      Position
  | Comp   Expr      Position
  | Neg    Expr      Position
  | Cast   Type Expr Position
  | Index  Expr Expr Position
  | ExprApply Apply  Position
  | Mem    Expr Name Position
  | MemInd Expr Name Position
  | SizeT  Type      Position
  | SizeE  Expr      Position
  deriving (Show, Eq)

{-
-- | Assignment expressions (aka. lvalues).
data AssignExpr
  = AssignExpr
  deriving (Show, Eq)
-}

-- | Function application.
data Apply = Apply Expr [Expr] deriving (Show, Eq)

instance Pos Stmt where
  posOf a = case a of
    Null -> undefined
    Compound _ _ p -> p
    TypeDecl _ _ p -> p
    VariableDef _ _ _ p -> p
    FunctionDef _ _ _ _ p -> p
    Assign _ _ p -> p
    StmtApply _ p -> p
    While _ _ p -> p
    If _ _ _ p -> p
    Return _ p -> p
    Goto _ p -> p
    Break p -> p
    Switch _ _ p -> p
    Case _ _ p -> p
    Default _ p -> p

instance Pos Expr where
  posOf a = case a of
    ConstInt   _ p -> p
    ConstFloat _ p -> p
    ConstChar  _ p -> p
    ConstString _ p -> p
    Var    _   p -> p
    Mul    _ _ p -> p 
    Div    _ _ p -> p 
    Rmd    _ _ p -> p 
    Add    _ _ p -> p 
    Sub    _ _ p -> p 
    Shl    _ _ p -> p 
    Shr    _ _ p -> p 
    Lt     _ _ p -> p 
    Gt     _ _ p -> p 
    Le     _ _ p -> p 
    Ge     _ _ p -> p 
    Eq     _ _ p -> p 
    Neq    _ _ p -> p 
    And    _ _ p -> p 
    Xor    _ _ p -> p 
    Or     _ _ p -> p 
    Adr    _   p -> p
    Ind    _   p -> p
    Minus  _   p -> p
    Comp   _   p -> p
    Neg    _   p -> p
    Cast   _ _ p -> p
    Index  _ _ p -> p
    Mem    _ _ p -> p
    MemInd _ _ p -> p
    SizeT  _   p -> p
    SizeE  _   p -> p
    ExprApply _ p -> p

-- | Parses a CIL program, given a file name and contents.
parseCIL :: String -> ByteString -> Stmt
parseCIL name code = case parseC code (initPos name) of
    Left e  -> error $ "parsing error: " ++ show e
    Right a -> cStat $ cTranslUnit a

-- | Rewrites a program to a single statement.
cTranslUnit :: CTranslUnit -> CStat
cTranslUnit (CTranslUnit items _) = CCompound [] (map f items ++ [CBlockStmt callMain]) none
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
cStat :: CStat -> Stmt
cStat a = case a of
  CLabel i a [] _ -> Compound [name i] [cStat a] p
  CCompound ids items _ -> Compound (map name ids) (map cBlockItem items) p
  CReturn Nothing _ -> Return Nothing p
  CReturn (Just a) _ -> Return (Just $ cExpr a) p
  CGoto (Ident name _ _) _ -> Goto name p
  CBreak _ -> Break p
  CWhile condition stmt False         _ -> While (cExpr condition) (cStat stmt) p
  CIf condition onTrue (Just onFalse) _ -> If (cExpr condition) (cStat onTrue) (cStat onFalse) p
  CIf condition onTrue Nothing        _ -> If (cExpr condition) (cStat onTrue)  Null           p
  CSwitch expr stmt _ -> Switch (cExpr expr) (cStat stmt) p
  CCase a b _ -> Case (cExpr a) (cStat b) p
  CDefault a _ -> Default (cStat a) p

  CExpr Nothing _ -> Null
  CExpr (Just (CAssign op a b n)) _ -> case op of
    CAssignOp -> Assign (cExpr a) (cExpr b) $ posOf n
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
    f :: CBinaryOp -> Stmt
    f op = cStat (CExpr (Just (CAssign CAssignOp a (CBinary op a b n) n)) n)

  CExpr (Just (CCall func args _)) n -> StmtApply (Apply (cExpr func) (map cExpr args)) $ posOf n

  CExpr (Just (CUnary op a n1)) n2
    | elem op [CPreIncOp, CPostIncOp] -> cStat $ CExpr (Just (CAssign CAddAssOp a one n1)) n2
    | elem op [CPreDecOp, CPostDecOp] -> cStat $ CExpr (Just (CAssign CSubAssOp a one n1)) n2
    where
    one = CConst $ CIntConst (cInteger 1) n1

  _ -> notSupported a "statement"
  where
  p = posOf a

cExpr :: CExpr -> Expr
cExpr a = case a of
  CConst (CIntConst i _)             -> ConstInt (fromIntegral $ getCInteger i) p
  CConst (CFloatConst (CFloat s) _)  -> ConstFloat (read s) p
  CConst (CCharConst (CChar a _) _)  -> ConstChar   a p
  CConst (CStrConst (CString a _) _) -> ConstString a p
  CVar (Ident name _ _)            _ -> Var name p
  CBinary CMulOp a b               _ -> Mul   (cExpr a) (cExpr b) p
  CBinary CDivOp a b               _ -> Div   (cExpr a) (cExpr b) p
  CBinary CRmdOp a b               _ -> Rmd   (cExpr a) (cExpr b) p
  CBinary CAddOp a b               _ -> Add   (cExpr a) (cExpr b) p
  CBinary CSubOp a b               _ -> Sub   (cExpr a) (cExpr b) p
  CBinary CShlOp a b               _ -> Shl   (cExpr a) (cExpr b) p
  CBinary CShrOp a b               _ -> Shr   (cExpr a) (cExpr b) p
  CBinary CLeOp  a b               _ -> Lt    (cExpr a) (cExpr b) p
  CBinary CGrOp  a b               _ -> Gt    (cExpr a) (cExpr b) p
  CBinary CLeqOp a b               _ -> Le    (cExpr a) (cExpr b) p
  CBinary CGeqOp a b               _ -> Ge    (cExpr a) (cExpr b) p
  CBinary CEqOp  a b               _ -> Eq    (cExpr a) (cExpr b) p
  CBinary CNeqOp a b               _ -> Neq   (cExpr a) (cExpr b) p
  CBinary CAndOp a b               _ -> And   (cExpr a) (cExpr b) p
  CBinary CXorOp a b               _ -> Xor   (cExpr a) (cExpr b) p
  CBinary COrOp  a b               _ -> Or    (cExpr a) (cExpr b) p
  CUnary CAdrOp  a                 _ -> Adr   (cExpr a) p
  CUnary CIndOp  a                 _ -> Ind   (cExpr a) p
  CUnary CMinOp  a                 _ -> Minus (cExpr a) p
  CUnary CCompOp a                 _ -> Comp  (cExpr a) p
  CUnary CNegOp  a                 _ -> Neg   (cExpr a) p
  CCast decl a                     _ -> Cast  (cDeclType decl) (cExpr a) p
  CIndex a b                       _ -> Index (cExpr a) (cExpr b) p
  CCall func args                  _ -> ExprApply (Apply (cExpr func) (map cExpr args)) p
  CMember a (Ident name _ _) False _ -> Mem    (cExpr a) name p
  CMember a (Ident name _ _) True  _ -> MemInd (cExpr a) name p
  CSizeofType a                    _ -> SizeT (cDeclType a) p
  CSizeofExpr a                    _ -> SizeE (cExpr a) p
  _ -> notSupported a "expressions"
  where
  p = posOf a

cBlockItem :: CBlockItem -> Stmt
cBlockItem a = case a of
  CBlockStmt    a -> cStat   a
  CBlockDecl    a -> cDecl   a
  CNestedFunDef a -> cFunDef a

cDecl :: CDecl -> Stmt
cDecl a = case a of
  CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just (Ident name _ _)) (Just decls) [] _) _)] [] _ -> TypeDecl name (structOrBitField decls) p
  CDecl [CTypeSpec (CSUType (CStruct CUnionTag  (Just (Ident name _ _)) (Just decls) [] _) _)] [] _ -> TypeDecl name (Union  $ map field decls) p
  CDecl [CTypeSpec (CEnumType (CEnum (Just (Ident name _ _)) (Just enums) [] _) _)] [] _ -> TypeDecl name (Enum [ (field, fromIntegral $ getCInteger i) | (Ident field _ _, Just (CConst (CIntConst i _))) <- enums ]) p
  CDecl (CStorageSpec (CTypedef _) : specs) a n -> TypeDecl name (Typedef $ cDeclType $ CDecl specs (init a) n) p
    where
    (Just (CDeclr (Just (Ident name _ _)) [] Nothing [] _), Nothing, Nothing) = last a
  CDecl _ [(Just (CDeclr _ (CFunDeclr _ _ _ : _) _ _ _), _, _)] _ -> Null  -- Ignore function prototypes.
  CDecl specs [(Just (CDeclr (Just (Ident name _ _)) _ Nothing [] _), init , Nothing)] _
    | any isExtern specs -> Null -- Ignore external variable decls.
    | otherwise ->  case init of
        Nothing   -> VariableDef name (cDeclType a) Nothing p
        Just init -> VariableDef name (cDeclType a) Nothing p --XXX
    where
    isExtern (CStorageSpec (CExtern _)) = True
    isExtern _ = False
  _ -> notSupported a "declaration"
  where
  p = posOf a

-- | A struct or a bit field.
structOrBitField :: [CDecl] -> Type
structOrBitField decls
  | all isField    decls = Struct $ map field decls
  | all isBitField decls = BitField typ $ map f decls
  | otherwise = notSupported (head decls) "inconsistent fields"
  where
  isField (CDecl _ [(Just (CDeclr (Just _) _ Nothing [] _), Nothing, Nothing)] _) = True
  isField _ = False
  typ = cDeclType $ head decls
  isBitField d@(CDecl _ [(Just (CDeclr (Just _) _ Nothing [] _), Nothing, Just (CConst (CIntConst _ _)))] _) = cDeclType d == typ
  isBitField _ = False
  f :: CDecl -> (Name, Int)
  f (CDecl _ [(Just (CDeclr (Just (Ident name _ _)) _ Nothing [] _), Nothing, Just (CConst (CIntConst cint _)))] _) = (name, fromIntegral $ getCInteger cint)
  f a = notSupported a "bit field format"

-- | Struct and union fields.
field :: CDecl -> (Name, Type)
field a = case a of
  d@(CDecl _ [(Just (CDeclr (Just (Ident name _ _)) _ Nothing [] _), Nothing, Nothing)] _) -> (name, cDeclType d)
  _ -> notSupported a "struct/union field"

-- | A 'Type' from a 'CDecl'.
cDeclType :: CDecl -> Type
cDeclType a = case a of
  CDecl specs [] _ -> cDeclSpec specs
  CDecl specs [(Just (CDeclr _ derived Nothing [] _), _, _)] _ -> foldr cDerivedDeclr (cDeclSpec specs) derived
  _ -> notSupported a "declaration type"

cDeclSpec :: [CDeclSpec] -> Type
cDeclSpec = cDeclSpec . sortBy f
  where
  f (CTypeQual _) _ = LT
  f _ (CTypeQual _) = GT
  f _ _             = EQ

  cDeclSpec a = case a of
    [] -> notSupported' a "empty type specification"
    [CTypeSpec (CVoidType _)] -> Void
    [CTypeSpec (CSUType (CStruct CStructTag (Just (Ident name _ _)) Nothing [] _) _)] -> StructRef  name
    [CTypeSpec (CSUType (CStruct CUnionTag  (Just (Ident name _ _)) Nothing [] _) _)] -> UnionRef   name
    [CTypeSpec (CEnumType (CEnum (Just (Ident name _ _))            Nothing [] _) _)] -> EnumRef    name
    [CTypeSpec (CTypeDef (Ident name _ _) _)]                                         -> TypedefRef name
    CStorageSpec (CTypedef _) : a -> Typedef  $ cDeclSpec a
    CTypeQual (CVolatQual _)  : a -> Volatile $ cDeclSpec a
    CTypeQual _               : a -> cDeclSpec a  -- Ignore other type qualifiers.
    CStorageSpec _            : a -> cDeclSpec a  -- Ignore storage specs.
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

cDerivedDeclr :: CDerivedDeclr -> Type -> Type
cDerivedDeclr a t = case a of
  CPtrDeclr quals _ -> volatile quals $ Ptr t
  CArrDeclr quals (CArrSize _ (CConst (CIntConst i _))) _ -> volatile quals $ Array (fromIntegral $ getCInteger i) t
  CFunDeclr (Right (decls, False)) [] _ -> Function t $ map cDeclType decls
  _ -> notSupported' a "derived declaration"
  where
  volatile quals typ = if any isVolatile quals then Volatile typ else typ
  isVolatile (CVolatQual _) = True
  isVolatile _              = False

cFunDef :: CFunDef -> Stmt
cFunDef (CFunDef specs (CDeclr (Just (Ident name _ _)) (CFunDeclr (Right (args', False)) [] _ : rest) Nothing [] _) [] stat n) = FunctionDef name (foldr cDerivedDeclr (cDeclSpec specs) rest) args (cStat stat) (posOf n)
  where
  args :: [(Name, Type)]
  args = [ (name, cDeclType decl) | decl@(CDecl _ [(Just (CDeclr (Just (Ident name _ _)) _ Nothing [] _), Nothing, Nothing)] _) <- args' ]
cFunDef a = notSupported a "function"
-- FunctionDecl Type Name [Type] Stmt Position

err :: (Pretty a, Pos a) => a -> String -> b
err a m = error $ position a ++ ": " ++ m ++ ": " ++ show (pretty a)

err' :: Pos a => a -> String -> b
err' a m = error $ position a ++ ": " ++ m

notSupported :: (Pretty a, Pos a) => a -> String -> b
notSupported a m = err a $ "not supported: " ++ m

notSupported' :: Pos a => a -> String -> b
notSupported' a m = err' a $ "not supported: " ++ m

-- | Format the file position of something with ties to the orignial source, like a 'Stmt'.
position :: Pos a => a -> String
position a = f ++ ":" ++ show l ++ ":" ++ show c
  where
  Position f l c = posOf a

