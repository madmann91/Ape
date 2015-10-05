module Ape.Analysis.BindingTime where

import qualified Ape.Expr as E
import Ape.Env

import Data.List

data Binding = Known | Unknown deriving (Eq, Show)

fromBinding :: Binding -> Bool
fromBinding Known = True
fromBinding _ = False

toBinding :: Bool -> Binding
toBinding True = Known
toBinding _ = Unknown

complexBinding :: Env Binding -> E.CExpr -> Env Binding
complexBinding f (E.If _ ifTrue ifFalse) = bindingTime (bindingTime f ifTrue) ifFalse
complexBinding f (E.Atomic (E.Val (E.Lambda arg _ body))) = bindingTime (insertEnv f arg Unknown) body
complexBinding f _ = f

bindingTime :: Env Binding -> E.Expr -> Env Binding
bindingTime e (E.Let v b) = bindingTime e' b
    where
        isKnown f val = case val of
            -- Tuples are known if one element of the tuple is known.
            -- This is because functions have only one argument which
            -- can be a tuple for functions that have multiple parameters.
            E.Tuple args -> foldl1' (||) $ map (isKnown f) args
            E.Var var -> lookupEnv f var == Known
            _ -> True

        bindingEnv oldEnv newEnv (w,_,c) = case c of
            E.Atomic (E.Val val) -> insertEnv newEnv w (toBinding $ isKnown oldEnv val)
            -- PrimOp is known if all the arguments are known
            E.Atomic (E.PrimOp _ vals) -> insertEnv newEnv w (allKnown oldEnv vals)
            E.App vals -> insertEnv newEnv w (allKnown oldEnv vals)
            _ -> insertEnv newEnv w Unknown

        allKnown env vals = toBinding $ foldl1' (&&) $ map (isKnown env) vals

        analyseBindings f = if result == f
            then f
            else analyseBindings result
            where
                result = foldl' (bindingEnv $ concatEnv e f) emptyEnv v

        -- Create sub-environment for these bindings, initially set to Unknown for everyone
        initEnv = foldl' (\f (w,_,_) -> insertEnv f w Unknown) emptyEnv v
        -- Fixpoint iteration to determine binding time
        localEnv = analyseBindings initEnv
        globalEnv = concatEnv e localEnv
        -- Iteration through the values bound by the let statement
        e' = foldl' (\f (_, _, c) -> complexBinding f c) globalEnv v

bindingTime e (E.Complex c) = complexBinding e c

