module Ape.Transform.Specialize where

import qualified Ape.Expr as E
import Ape.Env
import Ape.Transform.Substitute

-- Specialization beta expands every lambda with the given value list
specialize :: E.Expr -> [E.Value] -> E.Expr
specialize (E.Let v b) args = E.Let v (specialize b args)
specialize (E.Complex (E.Atomic (E.Val val))) args = specializeLambda val args
specialize expr _ = expr

specializeLambda :: E.Value -> [E.Value] -> E.Expr
specializeLambda (E.Lambda v _ b) (arg:args) = specialize (substitute e b) args
    where e = insertEnv emptyEnv v arg
specializeLambda val _ = E.Complex $ E.Atomic $ E.Val val

specializeApp :: Env E.Value -> E.CExpr -> E.Expr
specializeApp e (E.App (v:vs)) = specLambda
    where
        -- Get the lambda to specialize along with its name (if any)
        lambda = case v of
            E.Var n -> lookupEnv e n
            l -> l
        -- Generate a new specialized expression
        specLambda = specializeLambda lambda vs
specializeApp _ c = E.Complex $ c
