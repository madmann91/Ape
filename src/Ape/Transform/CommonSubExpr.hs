module Ape.Transform.CommonSubExpr (commonSubExpr, emptyExprMap) where

import Ape.Expr
import Ape.Env
import qualified Data.Map.Strict as M
import Ape.Transform.NormalizeExpr
import Data.List

type ExprMap = M.Map (CExpr Info) String

emptyExprMap :: ExprMap
emptyExprMap = M.empty

class CommonSubExpr a where
    commonSubExpr :: ExprMap -> Env Variable -> a Info -> a Info

-- Make all variables in the environment point their final renamed form
normalizeEnv :: Env Variable -> Env Variable
normalizeEnv e = if next == e
    then e
    else normalizeEnv next
    where next = mapEnv (\v -> if isInEnv e v then lookupEnv e v else v) e

instance CommonSubExpr Value where
    commonSubExpr _ e v@(Var i w) = if isInEnv e w then Var i $ lookupEnv e w else v
    commonSubExpr m e (Tuple i v) = Tuple i $ map (commonSubExpr m e) v
    commonSubExpr m e (Lambda i v t b) = Lambda i v t (commonSubExpr m e b)
    commonSubExpr _ _ v = v

instance CommonSubExpr CExpr where
    commonSubExpr m e c = case M.lookup c m of
        Just v -> Atomic $ Val $ Var (info c) v
        _ -> case c of
            If i b t f -> If i (commonSubExpr m e b) (commonSubExpr m e t) (commonSubExpr m e f)
            App i xs -> App i $ map (commonSubExpr m e) xs
            Atomic a -> Atomic (commonSubExpr m e a)

instance CommonSubExpr AExpr where
    commonSubExpr m e (Val v) = normalizeExpr $ Val $ commonSubExpr m e v
    commonSubExpr m e (PrimOp i op xs) = normalizeExpr $ PrimOp i op $ map (commonSubExpr m e) xs

instance CommonSubExpr Expr where
    commonSubExpr m e (Let i v b) = case v' of
        [] -> commonSubExpr m' e'' b
        _ -> Let i v' $ commonSubExpr m' e'' b
        where
            handleSingleBinding (expr2var, var2var, bindings) bind@(var, _, expr) = case expr of
                -- If the binding is under the form let a = b, we just remap a to b
                Atomic (Val (Var _ var')) -> (expr2var, insertEnv var2var var var', bindings)
                -- Otherwise, we need to check if the expression already exists somewhere else in the program
                _ -> case M.lookup expr expr2var of
                    Just var' -> (expr2var, insertEnv var2var var var', bindings)
                    Nothing   -> (M.insert expr var expr2var, var2var, bind:bindings)

            handleBindings (expr2var, var2var, bindings) = (expr2var', var2var', reverse bindings')
                where (expr2var', var2var', bindings') = foldl' handleSingleBinding (expr2var, var2var, []) bindings

            transformBindings prev@(expr2var, var2var, bindings) = if prev == result
                then result
                else transformBindings result
                where
                    bindings' = map (\(w, t, c) -> (w, t, commonSubExpr (M.delete c expr2var) var2var c)) bindings
                    result = handleBindings (m, var2var, bindings')

            -- Iterate until fixpoint is reached
            (m', e', v') = transformBindings $ handleBindings (m, e, v)
            e'' = normalizeEnv e'

    commonSubExpr m e (Complex c) = Complex $ commonSubExpr m e c
