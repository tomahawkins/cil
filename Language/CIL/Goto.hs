module Language.CIL.Goto (goto) where

import Language.CIL.StmtCore
import qualified Language.CIL.StmtGoto as G
import Language.CIL.Types

-- | Reduces a program to assignments, if statements, and goto jumps.
goto :: Stmt -> G.Stmt
goto _ = undefined

