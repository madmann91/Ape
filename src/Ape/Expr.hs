module Ape.Expr where

import qualified Ape.Type as Type

import Data.Int
import Data.Word

-- Memory access rights
data Access = ReadWrite | ReadOnly | WriteOnly deriving (Eq, Ord)

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
           deriving (Eq, Ord)

-- Variables, represented as IDs, must be unique within a module
type Variable = String

-- Comparison functions
data CmpOp = Equal | Greater | Less | GreaterEqual | LessEqual deriving (Eq, Ord)

-- Primitive operations
data Op = Add | Sub | Mul | Div
        | RShift | LShift
        | And | Or | Xor
        | Select
        | Cmp CmpOp
        | BitCast Type.Type
        | VectorElem Int
        | TupleElem Int
        deriving (Eq, Ord)

-- Complex expression, that may not terminate
data CExpr = If Value Expr Expr
           | App [Value]
           | Atomic AExpr
           deriving (Eq, Ord)

-- Atomic expression, that is guaranteed to terminate
data AExpr = Val Value
           | PrimOp Op [Value]
           deriving (Eq, Ord)

type LetBinding = (Variable, Type.Type, CExpr)

-- Expressions
data Expr = Let [LetBinding] Expr
          | Complex CExpr
          deriving (Eq, Ord)
