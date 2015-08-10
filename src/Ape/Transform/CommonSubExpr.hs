module Ape.Transform.CommonSubExpr (commonSubExpr, emptyExprMap) where

import Ape.Expr
import Ape.Env
import qualified Data.Map.Strict as M
import Data.List

type ExprMap = M.Map CExpr String

emptyExprMap :: ExprMap
emptyExprMap = M.empty

class CommonSubExpr a where
    commonSubExpr :: ExprMap -> Env Variable -> a -> a

instance CommonSubExpr Value where
    commonSubExpr _ e v@(Var w) = if isInEnv e w then Var $ lookupEnv e w else v
    commonSubExpr m e (Lambda v t b) = Lambda v t (commonSubExpr m e b)
    commonSubExpr _ _ v = v

instance CommonSubExpr CExpr where
    commonSubExpr m e c = case M.lookup c m of
        Just v -> Atomic $ Val $ Var v
        _ -> case c of
            If b t f -> If (commonSubExpr m e b) (commonSubExpr m e t) (commonSubExpr m e f)
            App xs -> App $ map (commonSubExpr m e) xs
            Atomic a -> Atomic (commonSubExpr m e a)

instance CommonSubExpr AExpr where
    commonSubExpr m e (Val v) = Val $ commonSubExpr m e v
    commonSubExpr m e (PrimOp op xs) = PrimOp op $ map (commonSubExpr m e) xs

instance CommonSubExpr Expr where
    commonSubExpr m e (Let v b) = case notFound of
        [] -> commonSubExpr m e' b
        _ -> Let notFound (commonSubExpr m' e b)
        where
            -- Apply common sub-expr to the bindings
            v' = map (\(w, t, c) -> (w, t, commonSubExpr m e c)) v
            -- Find the bindings that are in common
            (found, notFound) = partition (\(_, _, c) -> M.member c m) v'
            -- Create a new substitution environment
            e' = foldl (\f (w, _, c) -> insertEnv f w $ forceLookup c m) e found
            -- Add new variables to mapping
            m' = foldl (\n (w, _, c) -> M.insert c w n) m notFound
            forceLookup c n = case M.lookup c n of
                Just c' -> c'
                _ -> error "Cannot find value " ++ show(c) ++ " in common sub-expression map"
    commonSubExpr m e (Complex c) = Complex $ commonSubExpr m e c