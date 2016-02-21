module Ape.Transform.CommonSubExpr (commonSubExpr, emptyExprMap) where

import Ape.Expr
import Ape.Env
import qualified Data.Map.Strict as M
import Data.List

import Ape.Print
import Debug.Trace

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
    commonSubExpr m e (Let v b) = case v of
        [] -> commonSubExpr m' e' b
        _ -> Let v' (commonSubExpr m' e' b)
        where
            handleBinding (expr2var, var2var, bindings) bind@(var, _, expr) =
                case M.lookup expr expr2var of
                    Just var' | var' /= var -> (expr2var, insertEnv var2var var var', bindings)
                    _  -> (M.insert expr var expr2var, var2var, bind:bindings)

            handleBindings (expr2var, var2var, bindings) = (expr2var', var2var', reverse bindings')
                where
                    (expr2var', var2var', bindings') = foldl' handleBinding (expr2var, var2var, []) bindings

            transformBindings prev@(expr2var, var2var, bindings) = if prev == result
                then result
                else transformBindings result
                where
                    bindings' = map (\(w, t, c) -> (w, t, commonSubExpr (M.delete c expr2var) var2var c)) bindings
                    result = handleBindings (expr2var, var2var, bindings')

            -- Iterate until fixpoint is reached
            (m', e', v') = transformBindings $ handleBindings (m, e, v)

    commonSubExpr m e (Complex c) = Complex $ commonSubExpr m e c