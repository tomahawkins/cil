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
  | BitField Type [(Name, Int)]
  | StructRef  Name
  | UnionRef   Name
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

data Stmt
  = Null
  | Compound [Name] [Stmt] Position
  | TypeDecl Name Type Position
  deriving (Show, Eq)

instance Pos Stmt where
  posOf a = case a of
    Null           -> undefined
    Compound _ _ p -> p
    TypeDecl _ _ p -> p

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
  _ -> notSupported a "statement"
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
  CDecl _ [(Just (CDeclr _ [CFunDeclr _ _ _ ] _ _ _), _, _)] _ -> Null  -- Ignore function prototypes.
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
  CDecl [CTypeSpec (CVoidType _)] [] _ -> Void
  CDecl specs [(Just (CDeclr _ derived Nothing [] _), _, _)] _ -> foldr cDerivedDeclr (cDeclSpec specs) derived
  _ -> notSupported a "declaration type"

cDeclSpec :: [CDeclSpec] -> Type
cDeclSpec a = case a of
  [] -> Void
  [CTypeSpec (CVoidType _)] -> Void
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
cFunDef a = notSupported a "function"

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

