module Ape.Transform.Substitute (substituteWith, substitute) where

import Ape.Expr
import Ape.Env

class Substitute a where
    substituteWith :: (b -> Value) -> Env b -> a -> a
    substitute :: Env Value -> a -> a
    substitute = substituteWith id

instance Substitute Value where
    substituteWith f e x@(Var v) = if isInEnv e v
                             then f $ lookupEnv e v
                             else x
    substituteWith f e (Tuple v) = Tuple $ map (substituteWith f e) v
    substituteWith f e (Lambda v t b) = Lambda v t (substituteWith f e b)
    substituteWith _ _ x = x

instance Substitute CExpr where
    substituteWith f e (If c a b) = If (substituteWith f e c) (substituteWith f e a) (substituteWith f e b)
    substituteWith f e (App xs) = App $ map (substituteWith f e) xs
    substituteWith f e (Atomic a) = Atomic (substituteWith f e a)

instance Substitute AExpr where
    substituteWith f e (Val v) = Val $ substituteWith f e v
    substituteWith f e (PrimOp op xs) = PrimOp op $ map (substituteWith f e) xs

instance Substitute Expr where
    substituteWith f e (Let v b) = Let (map (\(i, t, b') -> (i, t, substituteWith f e b')) v) (substituteWith f e b)
    substituteWith f e (Complex c) = Complex (substituteWith f e c)
