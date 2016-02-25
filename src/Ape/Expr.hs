module Ape.Expr where

import qualified Ape.Type as T

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
           | Lambda Variable T.Type Expr
           deriving (Eq, Ord)

-- Variables, represented as IDs, must be unique within a module
type Variable = String

-- Comparison functions
data CmpOp = Equal | NotEqual | Greater | Less | GreaterEqual | LessEqual deriving (Eq, Ord)

-- Primitive operations
data Op = Add | Sub | Mul | Div
        | RShift | LShift
        | And | Or | Xor
        | Select
        | Cmp CmpOp
        | BitCast T.Type
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

type LetBinding = (Variable, T.Type, CExpr)

-- Expressions
data Expr = Let [LetBinding] Expr
          | Complex CExpr
          deriving (Eq, Ord)

class ExprSize a where
    exprSize :: a -> Int

instance ExprSize Expr where
    exprSize (Let v b) = 1 + exprSize b + (sum $ map (\(_,t,c) -> 1 + T.typeSize t + exprSize c) v)
    exprSize (Complex c) = 1 + exprSize c

instance ExprSize CExpr where
    exprSize (If c t f) = 1 + exprSize c + exprSize t + exprSize f
    exprSize (App v) = 1 + (sum $ map exprSize v)
    exprSize (Atomic a) = 1 + exprSize a

instance ExprSize AExpr where
    exprSize (Val v) = 1 + exprSize v
    exprSize (PrimOp _ v) = 1 + (sum $ map exprSize v)

instance ExprSize Value where
    exprSize (Lambda _ t b) = 1 + T.typeSize t + exprSize b
    exprSize (Tuple v) = 1 + length v
    exprSize _ = 1

isSymmetric :: Op -> Bool
isSymmetric Add = True
isSymmetric Mul = True
isSymmetric And = True
isSymmetric Or  = True
isSymmetric Xor = True
isSymmetric (Cmp Equal) = True
isSymmetric (Cmp NotEqual) = True
isSymmetric _ = False
