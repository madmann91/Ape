module Ape.Transform.CommonSubExpr (commonSubExpr) where

import Ape.Expr
import Ape.Env
import Ape.Transform.Substitute
import qualified Data.Map.Strict as M
import Data.Maybe

type ExprMap = M.Map CExpr String

commonSubExpr :: ExprMap -> Env CExpr -> Expr -> Expr
commonSubExpr m e (Let v b) =
    where
        match = mapMaybe (\(k, _, b') -> lookup b' m >>= (\s -> Just (k, s))) v

commonSubExpr m e x@(Complex _) = x