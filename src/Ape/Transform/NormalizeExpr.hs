module Ape.Transform.NormalizeExpr where

import Ape.Expr

class NormalizeExpr a where
    normalizeExpr :: a Info -> a Info

instance NormalizeExpr Expr where
    normalizeExpr (Let i v b) = Let i (map (\(w, t, c) -> (w, t, normalizeExpr c)) v) (normalizeExpr b)
    normalizeExpr (Complex c) = Complex (normalizeExpr c)

instance NormalizeExpr CExpr where
    normalizeExpr (If i c t f) = If i c (normalizeExpr t) (normalizeExpr f)
    normalizeExpr (App i v) = App i v
    normalizeExpr (Atomic a) = Atomic (normalizeExpr a)

-- Normalizes primops so that variables are always on the right, sorted by name.
-- Additionally, simplifies arithmetic expressions like 1 * x, o * x, 0 + x...
instance NormalizeExpr AExpr where
    normalizeExpr (PrimOp _ Add [x, y]) | isZero x = Val y
    normalizeExpr (PrimOp _ Add [x, y]) | isZero y = Val x
    normalizeExpr (PrimOp _ Mul [x, y]) | isOne x = Val y
    normalizeExpr (PrimOp _ Mul [x, y]) | isOne y = Val x
    normalizeExpr (PrimOp _ Mul [x, _]) | isZero x = Val x
    normalizeExpr (PrimOp _ Mul [_, y]) | isZero y = Val y
    normalizeExpr (PrimOp _ Div [x, y]) | isOne y = Val x
    normalizeExpr (PrimOp _ And [x, y]) | x == y = Val x
    normalizeExpr (PrimOp _ Or  [x, y]) | x == y = Val x
    normalizeExpr (PrimOp i Xor [x, y]) | x == y = Val $ zero i (typeInfo i)
    normalizeExpr (PrimOp i op [a, b]) | isSymmetric op && a > b = PrimOp i op [b, a]
    normalizeExpr (PrimOp i (Cmp Less) [a, b]) | a > b = PrimOp i (Cmp Greater) [b, a]
    normalizeExpr (PrimOp i (Cmp Greater) [a, b]) | a > b = PrimOp i (Cmp Less) [b, a]
    normalizeExpr (PrimOp i (Cmp LessEqual) [a, b]) | a > b = PrimOp i (Cmp GreaterEqual) [b, a]
    normalizeExpr (PrimOp i (Cmp GreaterEqual) [a, b]) | a > b = PrimOp i (Cmp LessEqual) [b, a]
    normalizeExpr e@_ = e

