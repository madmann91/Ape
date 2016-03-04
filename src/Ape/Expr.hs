module Ape.Expr where

import qualified Ape.Type as T

import Data.Int
import Data.Word

-- Values, result of the evaluation of a program
data Value a = I1  a [Bool]
             | I8  a [Int8]
             | I16 a [Int16]
             | I32 a [Int32]
             | I64 a [Int64]
             | U8  a [Word8]
             | U16 a [Word16]
             | U32 a [Word32]
             | U64 a [Word64]
             | F32 a [Float]
             | F64 a [Double]
             | Tuple a [Value a]
             | Var a Variable
             | Lambda a Variable T.Type (Expr a)
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
data CExpr a = If a (Value a) (Expr a) (Expr a)
             | App a [Value a]
             | Atomic (AExpr a)
             deriving (Eq, Ord)

-- Atomic expression, that is guaranteed to terminate
data AExpr a = Val (Value a)
             | PrimOp a Op [Value a]
             deriving (Eq, Ord)

type LetBinding a = (Variable, T.Type, CExpr a)

-- Expressions
data Expr a = Let a [LetBinding a] (Expr a)
            | Complex (CExpr a)
            deriving (Eq, Ord)

-- Annotation added to the AST
data Loc = Loc { rowBegin :: Int
               , colBegin :: Int
               , rowEnd :: Int
               , colEnd :: Int
               , file :: String
               } deriving (Show)
data Info = Info T.Type Loc

-- Dummy comparison for the annotation
instance Eq Info where
    (==) _ _ = True
instance Ord Info where
    compare _ _ = EQ

class Annotated a where
    info :: a b -> b
    updateInfo :: a b -> b -> a b

instance Annotated Expr where
    info (Let i _ _) = i
    info (Complex c) = info c
    updateInfo (Let _ v b) i = Let i v b
    updateInfo (Complex c) i = Complex $ updateInfo c i

instance Annotated CExpr where
    info (If i _ _ _) = i
    info (App i _) = i
    info (Atomic a) = info a
    updateInfo (If _ c t f) i = If i c t f
    updateInfo (App _ v) i = App i v
    updateInfo (Atomic a) i = Atomic $ updateInfo a i

instance Annotated AExpr where
    info (Val v) = info v
    info (PrimOp i _ _) = i
    updateInfo (Val v) i = Val $ updateInfo v i
    updateInfo (PrimOp _ op v) i = PrimOp i op v

instance Annotated Value where
    info (I1  i _) = i
    info (I8  i _) = i
    info (I16 i _) = i
    info (I32 i _) = i
    info (I64 i _) = i
    info (U8  i _) = i
    info (U16 i _) = i
    info (U32 i _) = i
    info (U64 i _) = i
    info (F32 i _) = i
    info (F64 i _) = i
    info (Tuple i _) = i
    info (Var i _)   = i
    info (Lambda i _ _ _) = i
    updateInfo (I1  _ v) i = I1  i v
    updateInfo (I8  _ v) i = I8  i v
    updateInfo (I16 _ v) i = I16 i v
    updateInfo (I32 _ v) i = I32 i v
    updateInfo (I64 _ v) i = I64 i v
    updateInfo (U8  _ v) i = U8  i v
    updateInfo (U16 _ v) i = U16 i v
    updateInfo (U32 _ v) i = U32 i v
    updateInfo (U64 _ v) i = U64 i v
    updateInfo (F32 _ v) i = F32 i v
    updateInfo (F64 _ v) i = F64 i v
    updateInfo (Tuple _ v) i = Tuple i v
    updateInfo (Var _ v) i = Var i v
    updateInfo (Lambda _ v t b) i = Lambda i v t b

locInfo :: Info -> Loc
locInfo (Info _ l) = l

typeInfo :: Info -> T.Type
typeInfo (Info t _) = t

updateLoc :: Info -> Loc -> Info
updateLoc (Info t _) l = Info t l

updateType :: Info -> T.Type -> Info
updateType (Info _ l) t = Info t l

class ExprSize a where
    exprSize :: a -> Int

instance ExprSize (Expr a) where
    exprSize (Let _ v b) = 1 + exprSize b + (sum $ map (\(_,t,c) -> 1 + T.typeSize t + exprSize c) v)
    exprSize (Complex c) = 1 + exprSize c

instance ExprSize (CExpr a) where
    exprSize (If _ c t f) = 1 + exprSize c + exprSize t + exprSize f
    exprSize (App _ v) = 1 + (sum $ map exprSize v)
    exprSize (Atomic a) = 1 + exprSize a

instance ExprSize (AExpr a) where
    exprSize (Val v) = 1 + exprSize v
    exprSize (PrimOp _ _ v) = 1 + (sum $ map exprSize v)

instance ExprSize (Value a) where
    exprSize (Lambda _ _ t b) = 1 + T.typeSize t + exprSize b
    exprSize (Tuple _ v) = 1 + length v
    exprSize _ = 1

isOne :: Value a -> Bool
isOne (I1  _ v) = all (==True) v
isOne (I8  _ v) = all (==1) v
isOne (I16 _ v) = all (==1) v
isOne (I32 _ v) = all (==1) v
isOne (I64 _ v) = all (==1) v
isOne (U8  _ v) = all (==1) v
isOne (U16 _ v) = all (==1) v
isOne (U32 _ v) = all (==1) v
isOne (U64 _ v) = all (==1) v
isOne (F32 _ v) = all (==1) v
isOne (F64 _ v) = all (==1) v
isOne _ = False

isZero :: Value a -> Bool
isZero (I1  _ v) = all (==False) v
isZero (I8  _ v) = all (==0) v
isZero (I16 _ v) = all (==0) v
isZero (I32 _ v) = all (==0) v
isZero (I64 _ v) = all (==0) v
isZero (U8  _ v) = all (==0) v
isZero (U16 _ v) = all (==0) v
isZero (U32 _ v) = all (==0) v
isZero (U64 _ v) = all (==0) v
isZero (F32 _ v) = all (==0) v
isZero (F64 _ v) = all (==0) v
isZero _ = False

isSymmetric :: Op -> Bool
isSymmetric Add = True
isSymmetric Mul = True
isSymmetric And = True
isSymmetric Or  = True
isSymmetric Xor = True
isSymmetric (Cmp Equal) = True
isSymmetric (Cmp NotEqual) = True
isSymmetric _ = False

zero :: a -> T.Type -> Value a
zero i (T.I1  n) = I1  i (replicate n False)
zero i (T.I8  n) = I8  i (replicate n 0)
zero i (T.I16 n) = I16 i (replicate n 0)
zero i (T.I32 n) = I32 i (replicate n 0)
zero i (T.I64 n) = I64 i (replicate n 0)
zero i (T.U8  n) = U8  i (replicate n 0)
zero i (T.U16 n) = U16 i (replicate n 0)
zero i (T.U32 n) = U32 i (replicate n 0)
zero i (T.U64 n) = U64 i (replicate n 0)
zero i (T.F32 n) = F32 i (replicate n 0)
zero i (T.F64 n) = F64 i (replicate n 0)
zero _ _ = error "Invalid type for zero"

one :: a -> T.Type -> Value a
one i (T.I1  n) = I1  i (replicate n True)
one i (T.I8  n) = I8  i (replicate n 1)
one i (T.I16 n) = I16 i (replicate n 1)
one i (T.I32 n) = I32 i (replicate n 1)
one i (T.I64 n) = I64 i (replicate n 1)
one i (T.U8  n) = U8  i (replicate n 1)
one i (T.U16 n) = U16 i (replicate n 1)
one i (T.U32 n) = U32 i (replicate n 1)
one i (T.U64 n) = U64 i (replicate n 1)
one i (T.F32 n) = F32 i (replicate n 1)
one i (T.F64 n) = F64 i (replicate n 1)
one _ _ = error "Invalid type for one"

