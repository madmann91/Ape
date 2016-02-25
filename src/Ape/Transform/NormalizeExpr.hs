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

-- Normalizes primops so that variables are always on the right, sorted by name
instance NormalizeExpr AExpr where
    normalizeExpr (PrimOp op [a, b]) | isSymmetric op && a > b = PrimOp op [b, a]
    normalizeExpr (PrimOp (Cmp Less) [a, b]) | a > b = PrimOp (Cmp Greater) [b, a]
    normalizeExpr (PrimOp (Cmp Greater) [a, b]) | a > b = PrimOp (Cmp Less) [b, a]
    normalizeExpr (PrimOp (Cmp LessEqual) [a, b]) | a > b = PrimOp (Cmp GreaterEqual) [b, a]
    normalizeExpr (PrimOp (Cmp GreaterEqual) [a, b]) | a > b = PrimOp (Cmp LessEqual) [b, a]
    normalizeExpr e@_ = e

