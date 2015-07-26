module Ape.Transform.Substitute (substitute) where

import Ape.Expr
import Ape.Env

class Substitute a where
    substitute :: Env Value -> a -> a

instance Substitute Value where
    substitute e x@(Var v) = if isInEnv e v
                             then lookupEnv e v
                             else x
    substitute e (Lambda v t b) = Lambda v t (substitute e b)
    substitute _ x = x

instance Substitute CExpr where
    substitute e (If c t f) = If (substitute e c) (substitute e t) (substitute e f)
    substitute e (App xs) = App $ map (substitute e) xs
    substitute e (Atomic a) = Atomic (substitute e a)

instance Substitute AExpr where
    substitute e (Val v) = Val $ substitute e v
    substitute e (PrimOp op xs) = PrimOp op $ map (substitute e) xs

instance Substitute Expr where
    substitute e (Let v b) = Let (map (\(i, t, b') -> (i, t, substitute e b')) v) (substitute e b)
    substitute e (Complex c) = Complex (substitute e c)