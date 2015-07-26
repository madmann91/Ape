module Ape.Expr where

import qualified Ape.Type as Type

import Data.Int
import Data.Word

-- Values, result of the evaluation of a program
data Value = I1  [Bool]
           | I8  [Int8]
           | I16 [Int16]
           | I32 [Int32]
           | I64 [Int64]
           | U8  [Word8]
           | U16 [Word16]
           | U32 [Word32]
           | U64 [Word64]
           | F32 [Float]
           | F64 [Double]
           | Tuple [Value]
           | Var Variable
           | Lambda Variable Type.Type Expr
           deriving (Show, Eq, Ord)

-- Variables, represented as IDs, must be unique within a module
type Variable = String

-- Comparison functions
data CmpOp = Equal | Greater | Less | GreaterEqual | LessEqual deriving (Show, Eq, Ord)

-- Primitive operations
data Op = Add | Sub | Mul | Div
        | RShift | LShift
        | And | Or | Xor
        | Select
        | Cmp CmpOp
        | BitCast Type.Type
        | VectorElem Int
        | TupleElem Int
        deriving (Show, Eq, Ord)

-- Complex expression, that may not terminate
data CExpr = If Value Expr Expr
           | App [Value]
           | Atomic AExpr
           deriving (Show, Eq, Ord)

-- Atomic expression, that is guaranteed to terminate
data AExpr = Val Value
           | PrimOp Op [Value]
           deriving (Show, Eq, Ord)

-- Expressions
data Expr = Let [(Variable, Type.Type, CExpr)] Expr
          | Complex CExpr
          deriving (Show, Eq, Ord)
