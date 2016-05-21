module Ape.Transform.Rename (rename) where

import Ape.Expr
import Ape.Env

renameEnv :: Env a -> Env String -> String -> (Env a, Env String, String)
renameEnv e f v = if isInEnv e v then (e', f', v') else (e, f, v)
	where
		v' = genVariable e v
		f' = insertEnv f v v'
		e' = insertEnv e v' (lookupEnv e v)

class Rename a where
    rename :: Env b -> Env String -> a c -> a c

instance Rename Value where
    rename _ f v@(Var i w) = if isInEnv f w
                             then Var i $ lookupEnv f w
                             else v
    rename e f (Tuple i  v) = Tuple i $ map (rename e f) v
    rename e f (Lambda i v t b) = Lambda i v' t (rename e' f' b)
    	where (e', f', v') = renameEnv e f v
    rename _ _ x = x

instance Rename CExpr where
    rename e f (If i c a b) = If i (rename e f c) (rename e f a) (rename e f b)
    rename e f (App i xs) = App i $ map (rename e f) xs
    rename e f (Atomic a) = Atomic (rename e f a)

instance Rename AExpr where
    rename e f (Val v) = Val $ rename e f v
    rename e f (PrimOp i op xs) = PrimOp i op $ map (rename e f) xs

instance Rename Expr where
    rename e f (Let i v b) = Let i v' b'
    	where
    		(e', f', v') = foldl updateBinding (e, f, []) v
    		b' = rename e' f' b
    		updateBinding (ke, kf, kv) (w, t, c) = (ke', kf', (w', t, c):kv)
    			where (ke', kf', w') = renameEnv ke kf w
    rename e f (Complex c) = Complex (rename e f c)
