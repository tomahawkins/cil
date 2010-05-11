module Main (main) where

import Data.Char
import Text.ParserCombinators.Poly.Plain

main :: IO ()
main = do
  f <- readFile "cil_types.mli"
  writeFile "cil_types_nocomments.mli" $ decomment f
  mapM_ print $ parseOCaml f

data Token
  = Type
  | Of
  | Eq
  | Pipe
  | Colon
  | SemiColon
  | Star
  | Comma
  | ParenLeft
  | ParenRight
  | BraceLeft
  | BraceRight
  | TConstructor String
  | TVariable    String
  | TParameter   String
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer = map t . filter (/= "mutable") . filter (not . null) . concatMap split . words . decomment
  where
  split :: String -> [String]
  split a = case break isSym a of
    (word, []) -> [word]
    (word, (a:b)) -> [word, [a]] ++ split b
  isSym = flip elem "=|;:*,(){}"
  t :: String -> Token
  t a = case a of
    "type" -> Type
    "and"  -> Type
    "of"   -> Of
    "="    -> Eq
    "|"    -> Pipe
    ":"    -> Colon
    ";"    -> SemiColon
    "*"    -> Star
    ","    -> Comma
    "("    -> ParenLeft
    ")"    -> ParenRight
    "{"    -> BraceLeft
    "}"    -> BraceRight
    '\'':a -> TParameter a
    a | elem '.' a -> t $ tail $ dropWhile (/= '.') a
    a:b    | isUpper a -> TConstructor (a : b)
           | otherwise -> TVariable    (a : b)
    a      -> error $ "unexpected string: " ++ a

decomment :: String -> String
decomment = decomment' 0
  where
  decomment' :: Int -> String -> String
  decomment' n "" | n == 0    = ""
                  | otherwise = error "reached end of file without closing comment"
  decomment' n ('(':'*':a) = "  " ++ decomment' (n + 1) a
  decomment' n ('*':')':a) | n > 0  = "  " ++ decomment' (n - 1) a
                           | otherwise = error "unexpected closing comment"
  decomment' n (a:b) = (if n > 0 && a /= '\n' then ' ' else a) : decomment' n b

data Type
  = Sum    TypeName [(String, [TypeRef])]
  | Record TypeName [(String, TypeRef)]
  | Alias  TypeName TypeRef
  deriving (Show, Eq)

data TypeName = TypeName String [String] deriving (Show, Eq) -- TypeName name params

data TypeRef
  = ApplyVariable  String TypeRef
  | ApplyParameter String TypeRef
  deriving (Show, Eq)

type OCaml = Parser Token

parseOCaml :: String -> [Type]
parseOCaml a = case runParser (many typeDef `discard` eof) $ lexer a of
  (Left msg, remaining) -> error $ msg ++ "\nremaining tokens: " ++ show (take 30 $ remaining) ++ " ..."
  (Right a, []) -> a
  (Right _, remaining) -> error $ "parsed, but with remaining tokens: " ++ show remaining

tok :: Token -> OCaml ()
tok a = satisfy (== a) >> return ()

parameter :: OCaml String
parameter = do
  a <- satisfy (\ a -> case a of { TParameter _ -> True; _ -> False })
  case a of
    TParameter s -> return s
    _ -> undefined

constructor :: OCaml String
constructor = do
  a <- satisfy (\ a -> case a of { TConstructor _ -> True; _ -> False })
  case a of
    TConstructor s -> return s
    _ -> undefined

variable :: OCaml String
variable = do
  a <- satisfy (\ a -> case a of { TVariable _ -> True; _ -> False })
  case a of
    TVariable s -> return s
    _ -> undefined

typeDef :: OCaml Type
typeDef = do { tok Type; n <- typeName; tok Eq; typeExpr n }

typeName :: OCaml TypeName
typeName = oneOf
  [ do { tok ParenLeft; p1 <- parameter; tok Comma; p2 <- parameter; ps <- many (tok Comma >> parameter); tok ParenRight; n <- variable; return $ TypeName n (p1:p2:ps) }
  , do { p <- parameter; n <- variable; return $ TypeName n [p] }
  , do { n <- variable; return $ TypeName n [] }
  ]

data VariableParameter = Var String | Param String

varOrParam :: OCaml VariableParameter
varOrParam = oneOf [variable >>= return . Var, parameter >>= return . Param]

varOrParams :: OCaml [VariableParameter]
varOrParams = do { a <- varOrParam; b <- many varOrParam;  return $ a : b }

typeRef :: OCaml TypeRef
typeRef = oneOf
  [ do { tok ParenLeft; varOrParams; tok Comma; varOrParams; many (tok Comma >> varOrParams); tok ParenRight; varOrParams; return () }
  , do { tok ParenLeft; typeRef; tok ParenRight; typeRef }
  , do { tok ParenLeft; typeRef; tok ParenRight; }
  , do { varOrParams; tok Star; typeRef; many (tok Star >> typeRef); return () }
  , do { varOrParams }
  ]

typeExpr :: TypeName -> OCaml Type
typeExpr n = oneOf
  [ recordType >>= return . Record n
  , sumType    >>= return . Sum    n
  , typeRef    >>= return . Alias  n
  ]

recordType :: OCaml [(String, TypeRef)]
recordType = do { tok BraceLeft; f  <- recordField; fs <- many (tok SemiColon >> recordField); optional $ tok SemiColon; tok BraceRight; return $ f : fs }

recordField :: OCaml (String, TypeRef)
recordField = do { n <- variable; tok Colon; t <- typeRef; return (n, t) }

sumType :: OCaml [(String, [TypeRef])]
sumType = do { optional (tok Pipe); a <- sumConstructor; b <- many (tok Pipe >> sumConstructor); return $ a : b }
 
sumConstructor :: OCaml (String, [TypeRef])
sumConstructor = oneOf
  [ do { n <- constructor; tok Of; a <- typeRef; tok Star; b <- typeRef; c <- many (tok Star >> typeRef); return (n, a:b:c) }
  , do { n <- constructor; tok Of; a <- typeRef; return (n, [a]) }
  , do { n <- constructor; return (n, []) }
  ]

