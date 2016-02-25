module Ape.Transform.NormalizeBindings where

import qualified Ape.Expr as E
import qualified Ape.Type as T
import Ape.Env

import qualified Data.Set as S
import Data.List

-- Renaming of the bindings is done in order to avoid name clashes
-- when pulling up a binding from a lower level
renameBindings :: S.Set String -> [E.LetBinding] -> (S.Set String, [E.LetBinding])
renameBindings variables = foldl' rename (variables, [])
    where
        isUsed s v = S.member v s
        rename (usedVars, bindings) (v, t, b) = (S.insert v' usedVars, (v', t, b) : bindings)
            where
                v' = genVariableWith (isUsed usedVars) v

-- Beta-expansion creates constructs such as let a = (let b = c in d) in e.
-- This is not allowed in ANF. Hence, we transform this expression into:
-- let a = d, b = c in e.
normalizeBindings :: Env a -> [(E.Variable, T.Type, E.Expr)] -> [E.LetBinding]
normalizeBindings e bindings = normalize names bindings []
    where
        -- Insert all the names in a set
        names = foldl' (\n (v, _) -> S.insert v n) S.empty (enumEnv e)
        normalize s (b:bs) ns = case b of
            -- Already normalized case
            (var, t, E.Complex c) -> normalize s bs ((var, t, c):ns)
            -- Normalization occurs here
            (var, t, E.Let vars body) -> normalize s' ((var, t, body):bs) (renamedVars ++ ns)
                where
                    (s', renamedVars) = renameBindings s vars
        normalize _ [] ns = reverse $ ns
