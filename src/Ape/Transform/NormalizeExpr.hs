module Ape.Transform.NormalizeExpr where

import Ape.Expr

class NormalizeExpr a where
    normalizeExpr :: a -> a

instance NormalizeExpr Expr where
    normalizeExpr (Let v b) = Let (map (\(w, t, c) -> (w, t, normalizeExpr c)) v) (normalizeExpr b)
    normalizeExpr (Complex c) = Complex (normalizeExpr c)

instance NormalizeExpr CExpr where
    normalizeExpr (If c t f) = If c (normalizeExpr t) (normalizeExpr f)
    normalizeExpr (Atomic a) = Atomic (normalizeExpr a)
    normalizeExpr (App v) = App v

-- Normalizes primops so that variables are always on the right, sorted by name.
-- Additionally, simplifies arithmetic expressions like 1 * x, o * x or 0 + x and x / 1.
instance NormalizeExpr AExpr where
    normalizeExpr (PrimOp Add [x, y]) | isZero x = Val y
    normalizeExpr (PrimOp Add [x, y]) | isZero y = Val x
    normalizeExpr (PrimOp Mul [x, y]) | isOne x = Val y
    normalizeExpr (PrimOp Mul [x, y]) | isOne y = Val x
    normalizeExpr (PrimOp Mul [x, _]) | isZero x = Val x
    normalizeExpr (PrimOp Mul [_, y]) | isZero y = Val y
    normalizeExpr (PrimOp Div [x, y]) | isOne y = Val x
    normalizeExpr (PrimOp op [a, b]) | isSymmetric op && a > b = PrimOp op [b, a]
    normalizeExpr (PrimOp (Cmp Less) [a, b]) | a > b = PrimOp (Cmp Greater) [b, a]
    normalizeExpr (PrimOp (Cmp Greater) [a, b]) | a > b = PrimOp (Cmp Less) [b, a]
    normalizeExpr (PrimOp (Cmp LessEqual) [a, b]) | a > b = PrimOp (Cmp GreaterEqual) [b, a]
    normalizeExpr (PrimOp (Cmp GreaterEqual) [a, b]) | a > b = PrimOp (Cmp LessEqual) [b, a]
    normalizeExpr e@_ = e

