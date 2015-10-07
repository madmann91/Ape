module Ape.Transform.PartialEval(partialEval) where

import qualified Ape.Expr as E
import Ape.Eval
import Ape.Env
import Ape.Transform.Substitute
import Ape.Transform.NormalizeBindings
import Ape.Transform.Specialize

import Data.List

-- Normalizes the given environment so that it only references itself from within lambdas
normalizeEnv :: Env E.Value -> Env E.Value
normalizeEnv e = if e == result
    then result
    else normalizeEnv result
    where
        e' = removeFromEnvIf e isLambda
        result = mapEnv (substitute e') e
        isLambda (E.Lambda _ _ _) = True
        isLambda _ = False

allKnown :: [E.Value] -> Bool
allKnown = and . map isKnown

isKnown :: E.Value -> Bool
isKnown (E.Var _) = False
isKnown (E.Tuple v) = allKnown v
isKnown _ = True

class PartialEval a where
    -- Partially evaluates an expression with an environment which contains known values
    partialEval :: Env E.Value -> a -> a

instance PartialEval E.Expr where
    partialEval e (E.Let v b) = case v' of
        [] -> b'
        _ -> E.Let v' b'
        where
            b' = partialEval e' b

            handleBindings :: (Env E.Value, [E.LetBinding]) -> (Env E.Value, [E.LetBinding])
            handleBindings prev@(var2val, bindings) = if result == prev
                then result
                else handleBindings result
                where
                    -- Partially evaluate bindings
                    evalBindings = map (\(w,t,c) -> (w, t, partialEvalComplex var2val c)) bindings
                    -- Monadic transformation on bindings to get CExprs instead of Exprs
                    normBindings = normalizeBindings evalBindings
                    -- Partition the evaluated bindings into values and expressions
                    isValue (_, _, E.Atomic (E.Val _)) = True
                    isValue _ = False
                    (values, bindings') = partition isValue normBindings
                    -- Store the values in the new environment
                    extractValue (E.Atomic (E.Val val)) = val
                    extractValue _ = error "Non-value given to extractValue"
                    var2val' = foldl' (\f (w, _, val) -> insertEnv f w $ extractValue val) var2val values
                    result = (normalizeEnv var2val', bindings')

            (e', v') = handleBindings (e, v)
    partialEval e (E.Complex c) = partialEvalComplex e c

instance PartialEval E.AExpr where
    partialEval e (E.PrimOp op ops) = if allKnown ops'
        then E.Val $ eval op ops'
        else E.PrimOp op ops'
        where
            ops' = map (substitute e) ops
    partialEval e (E.Val val) = E.Val $ partialEval e val

instance PartialEval E.Value where
    partialEval e (E.Lambda v t b) = E.Lambda v t $ partialEval e b
    partialEval e val = substitute e val

partialEvalComplex :: Env E.Value -> E.CExpr -> E.Expr
partialEvalComplex e (E.If c t f) = if known
    -- Evaluate if when the condition is known
    then if cond then partialEval e t else partialEval e f
    -- Do not evaluate if, but evaluate branches
    else E.Complex $ E.If c (partialEval e t) (partialEval e f)
    where
        (known, cond) = case c of
            E.I1 [cond'] -> (True, cond')
            E.Var var  -> (isInEnv e var, lookupEnv e var == E.I1 [True])
            _ -> (False, undefined)
partialEvalComplex e app@(E.App args) = if allKnown args
    then specializeApp e app
    else E.Complex app
partialEvalComplex e (E.Atomic a) = E.Complex $ E.Atomic $ partialEval e a

