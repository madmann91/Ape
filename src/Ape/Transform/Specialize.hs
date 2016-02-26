module Ape.Transform.Specialize where

import qualified Ape.Expr as E
import Ape.Env
import Ape.Transform.Substitute

-- Specialization beta expands every lambda with the given value list
specialize :: E.Expr E.Info -> [E.Value E.Info] -> E.Expr E.Info
specialize (E.Let i v b) args = E.Let i v (specialize b args)
specialize (E.Complex (E.Atomic (E.Val val))) args = specializeLambda val args
specialize expr _ = expr

specializeLambda :: E.Value E.Info -> [E.Value E.Info] -> E.Expr E.Info
specializeLambda (E.Lambda _ v _ b) (arg:args) = specialize (substitute e b) args
    where e = insertEnv emptyEnv v arg
specializeLambda val _ = E.Complex $ E.Atomic $ E.Val val

specializeApp :: Env (E.Value E.Info) -> E.CExpr E.Info -> E.Expr E.Info
specializeApp e (E.App _ (v:vs)) = specLambda
    where
        -- Get the lambda to specialize
        lambda = case v of
            E.Var _ n -> lookupEnv e n
            l -> l
        -- Generate a new specialized expression
        specLambda = specializeLambda lambda vs
specializeApp _ c = E.Complex $ c
