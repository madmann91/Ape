module Ape.Env where

import Ape.Print

import Data.Char
import qualified Data.StringMap as SM

type Env a = SM.StringMap a

emptyEnv :: Env a
emptyEnv = SM.empty

lookupEnv :: Env a -> String -> a
lookupEnv e i = case SM.lookup i e of
    Just v -> v
    Nothing -> error $ "Undeclared identifier " ++ i

insertEnv :: Env a -> String -> a -> Env a
insertEnv e "_" _ = e
insertEnv e i x = SM.insertWithKey (\k _ _ -> error $ "Identifier " ++ k ++ " already used") i x e

concatEnv :: Env a -> Env a -> Env a
concatEnv = SM.unionWithKey (\k _ _ -> error $ "Duplicate values for " ++ k)

isInEnv :: Env a -> String -> Bool
isInEnv e i = SM.member i e

enumEnv :: Env a -> [(String, a)]
enumEnv e = SM.toList e

removeFromEnv :: Env a -> String -> Env a
removeFromEnv e i = SM.delete i e

mapEnv :: (a-> b) -> Env a -> Env b
mapEnv = SM.map

mapEnvWithKey :: (String -> a -> b) -> Env a -> Env b
mapEnvWithKey = SM.mapWithKey

printEnv :: PrettyPrint a => Env a -> IO ()
printEnv e = print $ map (\(k, v) -> (k, prettyPrint0 v)) (enumEnv e)

genVariable :: Env a -> String -> String
genVariable e s = if SM.member s e
    then next
    else s
    where
        next = if isDigit c && c /= '9' then reverse $ chr (ord c + 1) : (tail $ reverse m) else m ++ "0"
        c = last m
        m = fst $ last $ SM.toListShortestFirst $ SM.prefixFilter s e
