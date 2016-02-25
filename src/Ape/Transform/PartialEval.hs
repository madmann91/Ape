module Ape.Transform.PartialEval(partialEval) where

import qualified Ape.Expr as E
import qualified Ape.Type as T
import Ape.Eval
import Ape.Env
import Ape.Transform.Substitute
import Ape.Transform.NormalizeBindings

import Data.List

type TypedValue = (T.Type, E.Value)
type TypedCExpr = (T.Type, E.CExpr)

class PartialEval a where
    -- Partially evaluates an expression with an environment which contains known values
    partialEval :: Env TypedValue -> a -> a

instance PartialEval E.Expr where
    partialEval e (E.Let v b) = case v' of
        [] -> b'
        _ -> E.Let v' b'
        where
            b' = partialEval e' b

            handleBindings :: (Env TypedValue, [E.LetBinding]) -> (Env TypedValue, [E.LetBinding])
            handleBindings prev@(var2val, bindings) = if result == prev
                then result
                else handleBindings result
                where
                    -- Partially evaluate bindings
                    evalBindings = map (\(w, t, c) -> (w, t, partialEvalComplex var2val c)) bindings
                    -- Monadic transformation on bindings to get CExprs instead of Exprs
                    normBindings = normalizeBindings var2val evalBindings
                    -- Partition the evaluated bindings into values and expressions
                    isValue (_, _, E.Atomic (E.Val _)) = True
                    isValue _ = False
                    (values, bindings') = partition isValue normBindings
                    -- Store the values in the new environment
                    extractValue (E.Atomic (E.Val val)) = val
                    extractValue _ = error "Non-value given to extractValue"
                    var2val' = foldl' (\f (w, t, val) -> insertEnv f w (t, extractValue val)) var2val values
                    result = (var2val', bindings')

            (e', v') = handleBindings (e, v)
            --ce' = foldl' (\f (w, t, c) -> insertEnv f w (t, c)) ce v
    partialEval e (E.Complex c) = partialEvalComplex e c

instance PartialEval E.AExpr where
    partialEval e (E.PrimOp op ops) = if allKnown ops
        then E.Val $ eval op ops
        else E.PrimOp op ops
        where
            allKnown = and . map isKnown
            isKnown (E.Var _) = False
            isKnown _ = True
    partialEval e (E.Val val) = E.Val $ partialEval e val

instance PartialEval E.Value where
    partialEval e (E.Lambda v t b) = E.Lambda v t $ partialEval e b
    partialEval _ val = val

partialEvalComplex :: Env TypedValue -> E.CExpr -> E.Expr
partialEvalComplex e branch@(E.If c t f) = if known
    -- Evaluate if when the condition is known
    then if cond then partialEval e t else partialEval e f
    else E.Complex $ branch
    where
        (known, cond) = case c of
            E.I1 [cond'] -> (True, cond')
            E.Var var  -> (True, isInEnv e var && snd (lookupEnv e var) == E.I1 [True])
            _ -> (False, undefined)
partialEvalComplex e app@(E.App vals) = E.Complex app
partialEvalComplex e (E.Atomic a) = E.Complex $ E.Atomic $ partialEval e (substituteWith snd e a)
