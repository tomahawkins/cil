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
  | Constructor String
  | Variable    String
  | Parameter   String
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
    '\'':a -> Parameter a
    a | elem '.' a -> t $ tail $ dropWhile (/= '.') a
    a:b    | isUpper a -> Constructor (a : b)
           | otherwise -> Variable    (a : b)
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
  = Sum    TypeName
  | Record TypeName
  | Tuple  TypeName
  deriving (Show, Eq)

data TypeName = TypeName String [String] deriving (Show, Eq) -- TypeName name params

data TypeExpr = TypeExpr deriving (Show, Eq)

type OCaml = Parser Token

parseOCaml :: String -> [()]
parseOCaml a = case runParser (many typeDef `discard` eof) $ lexer a of
  (Left msg, remaining) -> error $ msg ++ "\nremaining tokens: " ++ show (take 30 $ remaining) ++ " ..."
  (Right a, []) -> a
  (Right _, remaining) -> error $ "parsed, but with remaining tokens: " ++ show remaining

tok :: Token -> OCaml ()
tok a = satisfy (== a) >> return ()

parameter :: OCaml String
parameter = do
  a <- satisfy (\ a -> case a of { Parameter _ -> True; _ -> False })
  case a of
    Parameter s -> return s
    _ -> undefined

constructor :: OCaml String
constructor = do
  a <- satisfy (\ a -> case a of { Constructor _ -> True; _ -> False })
  case a of
    Constructor s -> return s
    _ -> undefined

variable :: OCaml String
variable = do
  a <- satisfy (\ a -> case a of { Variable _ -> True; _ -> False })
  case a of
    Variable s -> return s
    _ -> undefined

typeDef :: OCaml ()
typeDef = do { tok Type; typeName; tok Eq; typeExpr; return () }

typeName :: OCaml ()
typeName = oneOf
  [ do { tok ParenLeft; parameter; tok Comma; parameter; many (tok Comma >> parameter); tok ParenRight; variable; return () }
  , do { parameter; variable; return () }
  , do { variable; return () }
  ]

varOrParam :: OCaml ()
varOrParam = oneOf [variable >> return (), parameter >> return ()]

varOrParams :: OCaml ()
varOrParams = do { varOrParam >> many varOrParam >> return () }

typeRef :: OCaml ()
typeRef = oneOf
  [ do { tok ParenLeft; varOrParams; tok Comma; varOrParams; many (tok Comma >> varOrParams); tok ParenRight; varOrParams; return () }
  , do { tok ParenLeft; typeRef; tok ParenRight; typeRef }
  , do { tok ParenLeft; typeRef; tok ParenRight; }
  , do { varOrParams; tok Star; typeRef; many (tok Star >> typeRef); return () }
  , do { varOrParams }
  ]

typeExpr :: OCaml ()
typeExpr = oneOf [recordType, sumType, typeRef]

recordType :: OCaml ()
recordType = do
  tok BraceLeft
  f  <- recordField
  fs <- many (tok SemiColon >> recordField)
  optional $ tok SemiColon
  tok BraceRight

recordField :: OCaml ()
recordField = do
  variable
  tok Colon
  typeRef

sumType :: OCaml ()
sumType = optional (tok Pipe) >> sumConstructor >> many (tok Pipe >> sumConstructor) >> return ()
 
sumConstructor :: OCaml ()
sumConstructor = oneOf
  [ do { constructor; tok Of; typeRef; tok Star; typeRef; many (tok Star >> typeRef); return () }
  , do { constructor; tok Of; typeRef; return () }
  , constructor >> return ()
  ]

