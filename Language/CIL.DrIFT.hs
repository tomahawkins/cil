module Language.CIL
  ( File     (..)
  , Global   (..)
  , Fundec   (..)
  , Location (..)
  , parseC
  )
  where

import System.Exit
import System.Process
import Text.Parse

-- | File location.
data Location = Location String Int Int
  deriving (Show, Read) {-!derive : Parse !-}

-- | C file compilation unit.
data File = File String [Global] (Maybe Fundec) Bool -- ^ File name globals init initCalledFromMain
  deriving (Show, Read) {-!derive : Parse !-}

-- | Global definitions and declarations.
data Global
  = GFun Fundec Location     -- ^ Function declaration (prototype).
  | UnknownGlobal Location
  deriving (Show, Read) {-!derive : Parse !-}

-- | Function declaration.
data Fundec = Fundec --Varinfo [Varinfo] Block (Maybe Int) [Stmt] Funspec
  deriving (Show, Read) {-!derive : Parse !-}

-- | Variable info.
--data Varinfo = Varinfo String String

-- | Parse a C compilation unit (file).
parseC :: FilePath -> IO File
parseC file = do
  (exitCode, out, err) <- readProcessWithExitCode "frama-c" ["-dumpcil", file] ""
  let code = unlines $ tail $ lines out
  case exitCode of
    ExitSuccess -> return $ read code
    ExitFailure _ -> putStrLn err >> exitWith exitCode

  {-
  case exitCode of
    ExitSuccess -> case runParser parse code of
      (Left s,  a)  -> putStrLn ("parse error: " ++ s ++ "\n" ++ code ++ "\n" ++ a) >> exitFailure
      (Right f, _) ->  return f
    ExitFailure _ -> putStrLn err >> exitWith exitCode
  -}

