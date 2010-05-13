-- | Parses OCaml cil_types.mli file and generates an equivalent CIL.hs file and a supporting Frama-C plugin (-dumpcil).
module Main (main) where

import Data.Char
import Data.List
import Text.ParserCombinators.Poly.Plain
import Text.Printf

main :: IO ()
main = do
  f <- readFile "cil_types.mli"
  writeFile "cil_types_nocomments.mli" $ decomment f
  let types = parseOCaml f
  writeFile "CIL.hs"              $ haskellCIL    types
  writeFile "dumpcil/dump_cil.ml" $ dumpcilPlugin types


-- OCaml type types.

-- Type definitions.
data Type
  = Sum    [(String, [TypeRef])]
  | Record [(String, TypeRef)]
  | Alias  TypeRef
  deriving (Show, Eq)

--- Type definition name with parameters.
data TypeName = TypeName String [String] deriving (Show, Eq)

-- Type references.  Either type applications or tuples.
data TypeRef
  = Apply VarParam [TypeRef]
  | Tuple [TypeRef]
  deriving (Show, Eq)

-- Type variables or parameters.
data VarParam = Var String | Param String deriving (Show, Eq)



cap :: String -> String
cap [] = []
cap (a:b) = toUpper a : b

uncap :: String -> String
uncap [] = []
uncap (a:b) = toLower a : b

isAlias :: Type -> Bool
isAlias (Alias _) = True
isAlias _         = False

-- Haskell CIL module generation.
haskellCIL :: [(TypeName, Type)] -> String
haskellCIL types = unlines
  [ "-- | A Haskell interface to OCaml's CIL library, via Frama-C, providing both a simplied C AST and the ACSL specification language."
  , "module Language.CIL"
  , "  ( parseC"
  , unlines [ printf "  , %-26s %s" (cap name) (if isAlias t then "" else "(..)")  | (TypeName name _, t) <- types ]
  , "  )"
  , "  where"
  , ""
  , "import System.Exit"
  , "import System.Process"
  -- , "import Text.Parse"
  , ""
  , "-- | Parse a C compilation unit (file)."
  , "parseC :: FilePath -> IO File"
  , "parseC file = do"
  , "  (exitCode, out, err) <- readProcessWithExitCode \"frama-c\" [\"-dumpcil\", file] \"\""
  , "  let code = unlines $ tail $ lines out"
  , "  case exitCode of"
  , "    ExitSuccess -> return $ read code"
  , "    ExitFailure _ -> putStrLn err >> exitWith exitCode"
  , ""
  -- {-
  -- case exitCode of
  --   ExitSuccess -> case runParser parse code of
  --     (Left s,  a)  -> putStrLn ("parse error: " ++ s ++ "\n" ++ code ++ "\n" ++ a) >> exitFailure
  --     (Right f, _) ->  return f
  --   ExitFailure _ -> putStrLn err >> exitWith exitCode
  -- -}
  , "data Exn = Exn " ++ derives
  , "data Position = Position FilePath Int Int " ++ derives
  , "data Int64 = Int64 " ++ derives
  , ""
  , unlines [ printf "%s %s %s = %s %s" (if isAlias t then "type" else "data") (cap name) (intercalate " " params) (fType name t) (if isAlias t then "" else derives) | (TypeName name params, t) <- types ] 
  ]
  where
  derives = "deriving (Show, Read, Eq) {-! derive : Parse !-}"
  fType :: String -> Type -> String
  fType name t = case t of
    Sum    constructors -> intercalate " | " [ constructorName name ++ concat [ " (" ++ fTypeRef t ++ ")" | t <- args ] | (name, args) <- constructors ]
    Record fields -> printf "%s { %s }" (cap name) $ intercalate ", " [ printf "%s :: %s" field (fTypeRef tr) | (field, tr) <- fields ]
    Alias  tr     -> fTypeRef tr

  constructorName :: String -> String
  constructorName a = case a of
    "Block"   -> "Block'"
    "True"    -> "True'"
    "False"   -> "False'"
    "Nothing" -> "Nothing'"
    a         -> cap a

  fTypeRef :: TypeRef -> String
  fTypeRef a = case a of
    Apply a []   -> name a
    Apply (Var "list")   [a] -> "[" ++ fTypeRef a ++ "]"
    Apply (Var "option") [a] -> "Maybe (" ++ fTypeRef a ++ ")"
    Apply (Var "ref")    [a] -> fTypeRef a
    Apply a args -> name a ++ concat [ " (" ++ fTypeRef t ++ ")" | t <- args ]
    Tuple args   -> "(" ++ intercalate ", " (map fTypeRef args) ++ ")"
    where
    name (Var n)   = cap n
    name (Param n) = n


-- Frama-C 'dumpcil' plugin generation.
dumpcilPlugin :: [(TypeName, Type)] -> String
dumpcilPlugin types = unlines
  [ "open Ast"
  , "open Cil_types"
  , "open File"
  , "open Lexing"
  , "open List"
  , ""
  , "let list a = if length a == 0 then \"[]\" else \"[\" ^ String.concat \", \" a ^ \"]\""
  , ""
  , "let string a = \"\\\"\" ^ a ^ \"\\\"\" (*XXX Need to process meta chars. *)"
  , ""
  , "let unknown_location ="
  , "  let a = { pos_fname = \"unknown\""
  , "          ; pos_lnum = 0"
  , "          ; pos_bol  = 0"
  , "          ; pos_cnum = 0"
  , "          }"
  , "  in (a, a)"
  , ""
  , "let format_fundec _ = \"Fundec\""
  , ""
  , "let format_location (a, _) = \"Location\" ^ \" \" ^ string a.pos_fname"
  , "                                        ^ \" \" ^ string_of_int a.pos_lnum"
  , "                                        ^ \" \" ^ string_of_int (a.pos_cnum - a.pos_bol + 1)"
  , ""
  , "let unknown_global loc = \"UnknownGlobal (\" ^ format_location loc ^ \")\""
  , ""
  , "let format_global a = match a with"
  , "    GType        (a, loc)    -> unknown_global loc"
  , "  | GCompTag     (a, loc)    -> unknown_global loc"
  , "  | GCompTagDecl (a, loc)    -> unknown_global loc"
  , "  | GEnumTag     (a, loc)    -> unknown_global loc"
  , "  | GEnumTagDecl (a, loc)    -> unknown_global loc"
  , "  | GVarDecl     (f, v, loc) -> unknown_global loc"
  , "  | GVar         (v, i, loc) -> unknown_global loc"
  , "  | GFun         (a, loc)    -> \"GFun (\" ^ format_fundec a ^ \") (\" ^ format_location loc ^ \")\""
  , "  | GAsm         (_, loc)    -> unknown_global loc"
  , "  | GPragma      (a, loc)    -> unknown_global loc"
  , "  | GText        _           -> unknown_global unknown_location"
  , "  | GAnnot       (_, loc)    -> unknown_global loc"
  , ""
  , ""
  , "let format_file file ="
  , "  let globals = map format_global file.globals in"
  , "  \"File\""
  , "  ^ \" \" ^ string file.fileName"
  , "  ^ \" \" ^ list (map format_global file.globals)"
  , "  ^ \" \" ^ (match file.globinit with"
  , "            None   -> \"Nothing\""
  , "          | Some f -> \"(Just (\" ^ format_fundec f ^ \"))\")"
  , "  ^ \" \" ^ (if file.globinitcalled then \"True\" else \"False\")"
  , ""
  , "let run () ="
  , "  File.init_from_cmdline ();"
  , "  print_endline (format_file (Ast.get ()))"
  , ""
  , "module Self ="
  , "  Plugin.Register"
  , "    (struct"
  , "      let name = \"dumpcil\""
  , "      let shortname = \"dumpcil\""
  , "      let descr = \"Dumps CIL and ACSL to stdout to be read by Haskell CIL.\""
  , "    end);;"
  , ""
  , "module Enabled ="
  , "  Self.False"
  , "    (struct"
  , "      let option_name = \"-dumpcil\""
  , "      let descr = \"Dumps CIL and ACSL to stdout to be read by Haskell CIL.\""
  , "    end);;"
  , ""
  , "let () = Db.Main.extend (fun () -> if Enabled.get () then run ())"
  ]







-- Lexing.

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
    a:b    | isUpper a -> TConstructor (a : b)
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







-- Parsing.

type OCaml a = Parser Token a

parseOCaml :: String -> [(TypeName, Type)]
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
  a <- satisfy (\ a -> case a of { TConstructor _ -> True; _ -> False })
  case a of
    TConstructor s -> return s
    _ -> undefined

variable :: OCaml String
variable = do
  a <- satisfy (\ a -> case a of { Variable _ -> True; _ -> False })
  case a of
    Variable s -> return s
    _ -> undefined

typeDef :: OCaml (TypeName, Type)
typeDef = do { tok Type; n <- typeName; tok Eq; e <- typeExpr; return (n, e) }

typeName :: OCaml TypeName
typeName = oneOf
  [ do { tok ParenLeft; p1 <- parameter; tok Comma; p2 <- parameter; ps <- many (tok Comma >> parameter); tok ParenRight; n <- variable; return $ TypeName n (p1:p2:ps) }
  , do { p <- parameter; n <- variable; return $ TypeName n [p] }
  , do { n <- variable; return $ TypeName n [] }
  ]

varParams :: OCaml [VarParam]
varParams = do { a <- varParam; b <- many varParam;  return $ a : b }
  where
  varParam :: OCaml VarParam
  varParam = oneOf [variable >>= return . Var, parameter >>= return . Param]

typeRef :: OCaml TypeRef
typeRef = oneOf
  [ do { tok ParenLeft; a <- varParams; tok Comma; b <- varParams; c <- many (tok Comma >> varParams); tok ParenRight; d <- varParams; return $ apply (map (apply []) (a : b: c)) d }
  , do { tok ParenLeft; a <- typeRef; tok ParenRight; b <- varParams; return $ apply [a] b }
  , do { tok ParenLeft; a <- typeRef; tok ParenRight; return a }
  , do { a <- varParams; tok Star; b <- typeRef; c <- many (tok Star >> typeRef); return $ Tuple $  apply [] a : b : c }
  , do { a <- varParams; return $ apply [] a }
  ]
  where
  apply :: [TypeRef] -> [VarParam] -> TypeRef
  apply _ [] = error "typeRef.apply: no type to apply to"
  apply args [a] = Apply a args
  apply args (a:b) = apply [(apply args [a])] b

typeExpr :: OCaml Type
typeExpr = oneOf
  [ recordType >>= return . Record
  , sumType    >>= return . Sum
  , typeRef    >>= return . Alias
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

