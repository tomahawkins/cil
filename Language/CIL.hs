-- | Parsing the C Intermediate Language (CIL).
--   CIL provides a manageable means to analyze and compile C code.
--
--   The common method to reduce C to CIL is to use the cilly driver:
--
--   > cilly --merge --keepmerged { c-files-and-options }
--
-- <http://cil.sourceforge.net/>
module Language.CIL
  ( module Language.CIL.Parse
  , module Language.CIL.Types
  ) where

import Language.CIL.Parse
import Language.CIL.Types
