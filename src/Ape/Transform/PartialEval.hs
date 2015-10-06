module Ape.Transform.PartialEval(partialEval) where

import qualified Ape.Expr as E
import Ape.Eval
import Ape.Env
import Ape.Transform.Substitute
import Ape.Transform.NormalizeBindings

import Data.List

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
                    result = (var2val', bindings')

            (e', v') = handleBindings (e, v)
    partialEval e (E.Complex c) = partialEvalComplex e c

instance PartialEval E.AExpr where
    partialEval e prim@(E.PrimOp op vals) = if allKnown vals
        then E.Val $ eval op vals
        else prim
        where
            allKnown = and . map isKnown
            isKnown (E.Var _) = False
            isKnown _ = True
    partialEval _ a = a

partialEvalComplex :: Env E.Value -> E.CExpr -> E.Expr
partialEvalComplex e (E.If c t f) = if known
    -- Evaluate if when the condition is known
    then if cond then partialEval e t else partialEval e f
    -- Do not evaluate if, but evaluate branches
    else E.Complex $ E.If c (partialEval e t) (partialEval e f)
    where
        (known, cond) = case c of
            E.I1 [cond'] -> (True, cond')
            E.Var var  -> (True, isInEnv e var && lookupEnv e var == E.I1 [True])
            _ -> (False, undefined)
partialEvalComplex e app@(E.App vals) = E.Complex app
partialEvalComplex e (E.Atomic a) = E.Complex $ E.Atomic $ partialEval e (substitute e a)

