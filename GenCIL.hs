-- | Parses OCaml cil_types.mli file and generates an equivalent CIL.hs file and a supporting Frama-C plugin (-dumpcil).
module Main (main) where

import Data.Char
import Data.List hiding (group)
import System.Process
import Text.ParserCombinators.Poly.Plain
import Text.Printf

main :: IO ()
main = do
  f <- readFile "cil_types.mli"
  writeFile "cil_types_nocomments.mli" $ decomment f
  let types = parseOCaml f
  system "mkdir -p install-dumpcil-plugin"
  writeFile "install-dumpcil-plugin/Makefile"    $ dumpcilMakefile
  writeFile "install-dumpcil-plugin/dump_cil.ml" $ dumpcilPlugin types
  writeFile "CIL.hs" $ haskellCIL types


-- OCaml type types.

-- Type definitions.
data TypeDef
  = Sum    [(String, [TypeApply])]
  | Record [(String, TypeApply)]
  | Alias  TypeApply
  deriving (Show, Eq)

--- Type definition name with parameters.
data TypeName = TypeName String [String] deriving (Show, Eq)

-- Type references.  Either type applications or tuples.
data TypeApply = TypeApply VarPar [TypeApply]
  deriving (Show, Eq)

-- Type variables or parameters.
data VarPar = Var String | Par String deriving (Show, Eq)



cap :: String -> String
cap [] = []
cap (a:b) = toUpper a : b

isAlias :: TypeDef -> Bool
isAlias (Alias _) = True
isAlias _         = False

-- Haskell CIL module generation.
haskellCIL :: [(TypeName, TypeDef)] -> String
haskellCIL types = unlines
  [ "-- | A Haskell interface to OCaml's CIL library, via Frama-C, providing both a simplied C AST and the ACSL specification language."
  , "module Language.CIL"
  , "  ( parseC"
  , "  , debugParseC"
  , "  , installPlugin"
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
  , "-- | Prints output from frama-c -dumpcil."
  , "debugParseC :: FilePath -> IO ()"
  , "debugParseC file = do"
  , "  (exitCode, out, err) <- readProcessWithExitCode \"frama-c\" [\"-dumpcil\", file] \"\""
  , "  putStrLn out"
  , ""
  -- {-
  -- case exitCode of
  --   ExitSuccess -> case runParser parse code of
  --     (Left s,  a)  -> putStrLn ("parse error: " ++ s ++ "\n" ++ code ++ "\n" ++ a) >> exitFailure
  --     (Right f, _) ->  return f
  --   ExitFailure _ -> putStrLn err >> exitWith exitCode
  -- -}
  , " -- | Installs Frama-C '-dumpcil' plugin."
  , "installPlugin :: IO ()"
  , "installPlugin = do"
  , "  putStrLn \"creating install-dumpcil-plugin directory for plugin compiling and installation ...\""
  , "  system \"mkdir -p install-dumpcil-plugin\""
  , "  writeFile \"install-dumpcil-plugin/Makefile\" " ++ show dumpcilMakefile
  , "  writeFile \"install-dumpcil-plugin/dump_cil.ml\" " ++ show (dumpcilPlugin types)
  , "  putStrLn \"running 'make' to compile dumpcil plugin ...\""
  , "  system \"cd install-dumpcil-plugin && make\""
  , "  putStrLn \"running 'make install' to install dumpcil plugin ...\""
  , "  system \"cd install-dumpcil-plugin && make install\""
  , "  return ()"
  , ""
  , "data Exn = Exn " ++ derives
  , "data Position = Position FilePath Int Int " ++ derives
  , "data Int64 = Int64 " ++ derives
  , ""
  , unlines [ printf "%s %s %s = %s %s" (if isAlias t then "type" else "data") (cap name) (intercalate " " params) (fType name t) (if isAlias t then "" else derives) | (TypeName name params, t) <- types ] 
  ]
  where
  derives = "deriving (Show, Read, Eq) {-! derive : Parse !-}"
  fType :: String -> TypeDef -> String
  fType name t = case t of
    Sum    constructors -> intercalate " | " [ constructorName name ++ concat [ " " ++ group (fTypeApply t) | t <- args ] | (name, args) <- constructors ]
    Record fields -> printf "%s { %s }" (cap name) $ intercalate ", " [ printf "%s :: %s" field (fTypeApply tr) | (field, tr) <- fields ]
    Alias  tr     -> fTypeApply tr

  fTypeApply :: TypeApply -> String
  fTypeApply a = case a of
    TypeApply a []   -> name a
    TypeApply (Var "list")   [a] -> "[" ++ fTypeApply a ++ "]"
    TypeApply (Var "option") [a] -> "Maybe " ++ group (fTypeApply a)
    TypeApply (Var "ref")    [a] -> fTypeApply a
    TypeApply (Var "tuple") args -> group $ intercalate ", " (map fTypeApply args)
    TypeApply a args -> name a ++ concat [ " (" ++ fTypeApply t ++ ")" | t <- args ]
    where
    name (Var n) = cap n
    name (Par n) = n

constructorName :: String -> String
constructorName a = case a of
  "Block"   -> "Block'"
  "True"    -> "True'"
  "False"   -> "False'"
  "Nothing" -> "Nothing'"
  a         -> cap a


-- Frama-C 'dumpcil' plugin generation.
dumpcilPlugin :: [(TypeName, TypeDef)] -> String
dumpcilPlugin types = unlines
  [ "open Ast"
  , "open Cil_types"
  , "open File"
  , "open Lexing"
  , "open List"
  , "open String"
  , ""
  , "let string a = \"\\\"\" ^ a ^ \"\\\"\" (* XXX Doesn't handle '\\' or '\"' chars in string. *)"
  , "let bool a = if a then \"True\" else \"False\""
  , "let position t = \"Position \\\"\" ^ t.pos_fname ^ \"\\\" \" ^ string_of_int t.pos_lnum ^ \" \" ^ string_of_int (t.pos_cnum - t.pos_bol + 1)"
  , ""
  , "let rec " ++ intercalate "\nand " (map fType types)
  , ""
  , "let run () ="
  , "  File.init_from_cmdline ();"
  , "  print_endline (file (Ast.get ()))"
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
  where
  fType :: (TypeName, TypeDef) -> String
  fType (TypeName name args, m) = name ++ concatMap (" " ++) args ++ " m = " ++ case m of  -- Parametric types are passed formating functions for each of the parameters.
    Sum constructors -> function (map constructor constructors) ++ " m"
    Record fields -> "\"" ++ cap name ++ " { " ++ concat (map field fields) ++ " }\""
    Alias m -> fTypeApply m ++ " m"

  constructor :: (String, [TypeApply]) -> (String, String)
  constructor (name, [])   = (name, show $ cap name)
  constructor (name, [m])  = (name ++ " m1", show (constructorName name) ++ " ^ " ++ fTypeApply m ++ " m1")
  constructor (name, args) = (name ++ group (intercalate ", " [ "m" ++ show i | i <- [1 .. length args] ]), show (constructorName name) ++ concat [ " ^ \" \" ^ " ++ fTypeApply m ++ " m" ++ show i | (m, i) <- zip args [1..] ])

  field :: (String, TypeApply) -> String
  field (name, t) = name ++ " = " ++ "\" ^ " ++ fTypeApply t ++ " m." ++ name ++ " ^ \""

  -- Returns an OCaml function :: Data -> String
  fTypeApply :: TypeApply -> String
  fTypeApply m = case m of
    TypeApply (Var "int")    [] -> "string_of_int"
    TypeApply (Var "exn")    [] -> "(fun _ -> \"Exn\")"
    TypeApply m [] -> name m  -- Vars are top level functions, Params should be functions passed in and thus in scope.
    TypeApply (Var "list")   [m] -> function [("m", "\"[\" ^ concat \", \" (map " ++ group (fTypeApply m) ++ " m) ^ \"]\"")]
    TypeApply (Var "option") [m] -> function [("None", show "Nothing"), ("Some m", show "Just " ++ " ^ " ++ group (fTypeApply m) ++ " m")]
    TypeApply (Var "ref")    [m] -> fTypeApply m
    TypeApply (Var "tuple") args -> function [(group $ intercalate ", " [ "m" ++ show i | i <- [1 .. length args] ], metaGroup $ intercalate " ^ \",\" ^ " [ group (fTypeApply m) ++ " m" ++ show i | (m, i) <- zip args [1..] ])]
    TypeApply m args -> group $ name m ++ concatMap ((" " ++) . group . fTypeApply) args
    where
    name (Var n) = n
    name (Par n) = n

  function :: [(String, String)] -> String
  function matches = group $ "function " ++ intercalate " | " [ a ++ " -> " ++ b | (a, b) <- matches ]

group :: String -> String
group a = "(" ++ a ++ ")"

metaGroup :: String -> String
metaGroup a = "\"(\" ^ " ++ a ++ " ^ \")\""


-- | Makefile used to install the dumpcil plugin.
dumpcilMakefile :: String
dumpcilMakefile = unlines
  [ "FRAMAC_SHARE  :=$(shell frama-c.byte -print-path)"
  , "FRAMAC_LIBDIR :=$(shell frama-c.byte -print-libpath)"
  , "PLUGIN_NAME = Dumpcil"
  , "PLUGIN_CMO  = dump_cil"
  , "include $(FRAMAC_SHARE)/Makefile.dynamic"
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







-- Parsing.

type OCaml a = Parser Token a

parseOCaml :: String -> [(TypeName, TypeDef)]
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

typeDef :: OCaml (TypeName, TypeDef)
typeDef = do { tok Type; n <- typeName; tok Eq; e <- typeExpr; return (n, e) }

typeName :: OCaml TypeName
typeName = oneOf
  [ do { tok ParenLeft; a <- parameter; b <- many1 (tok Comma >> parameter); tok ParenRight; n <- variable; return $ TypeName n $ a : b }
  , do { p <- parameter; n <- variable; return $ TypeName n [p] }
  , do { n <- variable; return $ TypeName n [] }
  ]

varPars :: OCaml [VarPar]
varPars = do { a <- varPar; b <- many varPar;  return $ a : b }
  where
  varPar :: OCaml VarPar
  varPar = oneOf [variable >>= return . Var, parameter >>= return . Par]

typeApply :: OCaml TypeApply
typeApply = oneOf
  [ do { tok ParenLeft; a <- varPars; b <- many1 (tok Comma >> varPars); tok ParenRight; c <- varPars; return $ apply (map (apply []) (a : b)) c }
  , do { tok ParenLeft; a <- typeApply; tok ParenRight; b <- varPars; return $ apply [a] b }
  , do { tok ParenLeft; a <- typeApply; tok ParenRight; return a }
  , do { a <- varPars; b <- many1 (tok Star >> typeApply); return $ TypeApply (Var "tuple") $ apply [] a : b }
  , do { a <- varPars; return $ apply [] a }
  ]
  where
  apply :: [TypeApply] -> [VarPar] -> TypeApply
  apply _ [] = error "typeApply: no type to apply to"
  apply args [a] = TypeApply a args
  apply args (a:b) = apply [(apply args [a])] b

typeExpr :: OCaml TypeDef
typeExpr = oneOf
  [ recordType >>= return . Record
  , sumType    >>= return . Sum
  , typeApply  >>= return . Alias
  ]

recordType :: OCaml [(String, TypeApply)]
recordType = do { tok BraceLeft; f  <- recordField; fs <- many (tok SemiColon >> recordField); optional $ tok SemiColon; tok BraceRight; return $ f : fs }

recordField :: OCaml (String, TypeApply)
recordField = do { n <- variable; tok Colon; t <- typeApply; return (n, t) }

sumType :: OCaml [(String, [TypeApply])]
sumType = do { optional (tok Pipe); a <- sumConstructor; b <- many (tok Pipe >> sumConstructor); return $ a : b }
 
sumConstructor :: OCaml (String, [TypeApply])
sumConstructor = oneOf
  [ do { n <- constructor; tok Of; a <- typeApply; b <- many1 (tok Star >> typeApply); return (n, a:b) }
  , do { n <- constructor; tok Of; a <- typeApply; return (n, [a]) }
  , do { n <- constructor; return (n, []) }
  ]

