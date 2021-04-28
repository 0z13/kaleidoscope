module Codegen where

import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST
import Data.Map as M
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP
-- And we sang, "Here we go again". 
-- Yes, i love james blunt

-- A IEEE magic number
double :: Type
double = FloatingPointType DoubleFP 

-- Pretty confused.

type SymbolTable = [(String, Operand)]

data CodegenState = CodeGenState {
  currentBlock :: Name
, blocks       :: Map.Map 
, symtab       :: SymbolTable
, blockCount   :: It
, count        :: Word
, names        :: Names 
} 
