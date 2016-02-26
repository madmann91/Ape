module Ape.Transform.PartialEval(partialEval) where

import qualified Ape.Expr as E
import Ape.Eval
import Ape.Env
import Ape.Transform.Substitute
import Ape.Transform.NormalizeBindings
import Ape.Transform.Specialize

import Data.List

-- Normalizes the given environment so that it only references itself from within lambdas
normalizeEnv :: Env (E.Value E.Info) -> Env (E.Value E.Info)
normalizeEnv e = if e == result
    then result
    else normalizeEnv result
    where
        e' = removeFromEnvIf e isLambda
        result = mapEnv (substitute e') e
        isLambda (E.Lambda _ _ _ _) = True
        isLambda _ = False

allKnown :: [E.Value E.Info] -> Bool
allKnown = and . map isKnown

isKnown :: E.Value E.Info -> Bool
isKnown (E.Var _ _) = False
isKnown (E.Tuple _ v) = allKnown v
isKnown _ = True

class PartialEval a where
    -- Partially evaluates an expression with an environment which contains known values
    partialEval :: Env (E.Value E.Info) -> a E.Info -> a E.Info

instance PartialEval E.Expr where
    partialEval e (E.Let i v b) = case v' of
        [] -> b'
        _ -> E.Let i v' b'
        where
            b' = partialEval e' b

            handleBindings :: (Env (E.Value E.Info), [E.LetBinding E.Info]) -> (Env (E.Value E.Info), [E.LetBinding E.Info])
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
                    var2val' = foldl' (\f (w, _, val) -> insertEnv f w $ extractValue val) var2val values
                    result = (normalizeEnv var2val', bindings')

            (e', v') = handleBindings (e, v)
    partialEval e (E.Complex c) = partialEvalComplex e c

instance PartialEval E.AExpr where
    partialEval e (E.PrimOp i op ops) = if allKnown ops'
        then E.Val $ eval i op ops'
        else E.PrimOp i op ops'
        where
            ops' = map (substitute e) ops
    partialEval e (E.Val val) = E.Val $ partialEval e val

instance PartialEval E.Value where
    partialEval e (E.Lambda i v t b) = E.Lambda i v t $ partialEval e b
    partialEval e val = substitute e val

partialEvalComplex :: Env (E.Value E.Info) -> E.CExpr E.Info -> E.Expr E.Info
partialEvalComplex e branch@(E.If _ c t f) = if known
    -- Evaluate if when the condition is known
    then if cond then partialEval e t else partialEval e f
    else E.Complex $ branch
    where
        (known, cond) = case c of
            E.I1 _ [cond'] -> (True, cond')
            E.Var _ var  -> (isInEnv e var, isTrue $ lookupEnv e var)
            _ -> (False, undefined)
        isTrue (E.I1 _ [True]) = True
        isTrue _ = False
partialEvalComplex e (E.App i args) = if allKnown (tail args')
    then partialEval e $ specializeApp e (E.App i args')
    else E.Complex $ E.App i args'
    where
        args' = (head args) : (map (substitute e) $ tail args)
partialEvalComplex e (E.Atomic a) = E.Complex $ E.Atomic $ partialEval e a
