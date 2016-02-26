module Ape.Transform.Substitute (substituteWith, substitute) where

import Ape.Expr
import Ape.Env

class Substitute a where
    substituteWith :: (b -> Value c) -> Env b -> a c -> a c
    substitute :: Env (Value c) -> a c -> a c
    substitute = substituteWith id

instance Substitute Value where
    substituteWith f e x@(Var _ v) = if isInEnv e v
                             then f $ lookupEnv e v
                             else x
    substituteWith f e (Tuple i  v) = Tuple i $ map (substituteWith f e) v
    substituteWith f e (Lambda i v t b) = Lambda i v t (substituteWith f e b)
    substituteWith _ _ x = x

instance Substitute CExpr where
    substituteWith f e (If i c a b) = If i (substituteWith f e c) (substituteWith f e a) (substituteWith f e b)
    substituteWith f e (App i xs) = App i $ map (substituteWith f e) xs
    substituteWith f e (Atomic a) = Atomic (substituteWith f e a)

instance Substitute AExpr where
    substituteWith f e (Val v) = Val $ substituteWith f e v
    substituteWith f e (PrimOp i op xs) = PrimOp i op $ map (substituteWith f e) xs

instance Substitute Expr where
    substituteWith f e (Let i v b) = Let i (map (\(w, t, b') -> (w, t, substituteWith f e b')) v) (substituteWith f e b)
    substituteWith f e (Complex c) = Complex (substituteWith f e c)
