module Ape.Transform.NormalizeBindings where

import qualified Ape.Expr as E
import qualified Ape.Type as T

-- Beta-expansion creates constructs such as let a = (let b = c in d) in e.
-- This is not allowed in ANF. Hence, we transform this expression into:
-- let a = d, b = c in e.
normalizeBindings :: [(E.Variable, T.Type, E.Expr)] -> [E.LetBinding]
normalizeBindings bindings = normalize bindings []
    where
        normalize (b:bs) ns = case b of
            -- Already normalized case
            (var, t, E.Complex c) -> normalize bs ((var, t, c):ns)
            -- Normalization occurs here
            (var, t, E.Let vars body) -> normalize ((var, t, body):bs) (vars ++ ns)
        normalize [] ns = reverse $ ns

