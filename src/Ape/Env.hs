module Ape.Env where

import Data.Char
import qualified Data.StringMap as SM

type Env a = SM.StringMap a

emptyEnv :: Env a
emptyEnv = SM.empty

lookupEnv :: Env a -> String -> a
lookupEnv e i = e SM.! i

insertEnv :: Env a -> String -> a -> Env a
insertEnv e "_" _ = e
insertEnv e i x = SM.insertWithKey (\k _ _ -> error $ "Identifier " ++ (show k) ++ " already used") i x e

isInEnv :: Env a -> String -> Bool
isInEnv e i = SM.member i e

enumEnv :: Env a -> [(String, a)]
enumEnv e = SM.toList e

genVariable :: Env a -> String -> String
genVariable e s = if SM.member s e
    then next
    else s
    where
        next = if c == '9' then m ++ "0" else reverse $ chr (ord c + 1) : tail (reverse m)
        c = last m
        m = last $ map fst $ SM.toListShortestFirst $ SM.prefixFilter s e
