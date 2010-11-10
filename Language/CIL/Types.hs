module Language.CIL.Types
  ( Name
  , Type  (..)
  , Expr  (..)
  , Init  (..)
  , Apply (..)
  ) where

import Language.C hiding (Name)

-- | Identifiers.
type Name = String

-- | Types.
data Type
  = Void
  | Array Int Type
  | Ptr Type
  | Volatile Type                -- ^ A volatile qualified type.
  | Typedef Type
  | Struct [(Name, Type)]
  | Union  [(Name, Type)]
  | Enum   [(Name, Int)]
  | BitField Type [(Name, Int)]
  | StructRef  Name              -- ^ Reference to a struct type.
  | UnionRef   Name              -- ^ Reference to a union type.
  | EnumRef    Name              -- ^ Reference to an enum type.
  | TypedefRef Name              -- ^ Reference to a previously defined typedef.
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

-- | Expressions.
data Expr
  = ConstInt    Int    Position
  | ConstFloat  Double Position
  | ConstChar   Char   Position
  | ConstString String Position
  | Var    Name      Position  -- ^ Variable reference.
  | Mul    Expr Expr Position  -- ^ a * b
  | Div    Expr Expr Position  -- ^ a / b
  | Rmd    Expr Expr Position  -- ^ a % b
  | Add    Expr Expr Position  -- ^ a + b
  | Sub    Expr Expr Position  -- ^ a - b
  | Shl    Expr Expr Position  -- ^ a << b
  | Shr    Expr Expr Position  -- ^ a >> b
  | Lt     Expr Expr Position  -- ^ a < b
  | Gt     Expr Expr Position  -- ^ a > b
  | Le     Expr Expr Position  -- ^ a <= b
  | Ge     Expr Expr Position  -- ^ a >= b
  | Eq     Expr Expr Position  -- ^ a == b
  | Neq    Expr Expr Position  -- ^ a != b
  | And    Expr Expr Position  -- ^ a & b
  | Xor    Expr Expr Position  -- ^ a ^ b
  | Or     Expr Expr Position  -- ^ a | b
  | Adr    Expr      Position  -- ^ &a
  | Ind    Expr      Position  -- ^ \*a
  | Minus  Expr      Position  -- ^ \-a
  | Comp   Expr      Position  -- ^ ~a
  | Neg    Expr      Position  -- ^ !a
  | Cast   Type Expr Position  -- ^ (...) a
  | Index  Expr Expr Position  -- ^ a[b]
  | Mem    Expr Name Position  -- ^ a.name
  | MemInd Expr Name Position  -- ^ a->name
  | SizeT  Type      Position  -- ^ sizeof(type)
  | SizeE  Expr      Position  -- ^ sizeof(expr)
  deriving (Show, Eq)

-- | Initialization expressions.
data Init
  = Init Expr
  | InitList [Init]
  deriving (Show, Eq)

-- | Function application.
data Apply = Apply Expr [Expr] deriving (Show, Eq)

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

