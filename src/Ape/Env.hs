module Ape.Env where

import Ape.Print

import Data.Char
import qualified Data.Map.Strict as M

type Env a = M.Map String a

emptyEnv :: Env a
emptyEnv = M.empty

lookupEnv :: Env a -> String -> a
lookupEnv e i = case M.lookup i e of
    Just v -> v
    Nothing -> error $ "Undeclared identifier " ++ i

insertEnv :: Env a -> String -> a -> Env a
insertEnv e "_" _ = e
insertEnv e i x = M.insertWithKey (\k _ _ -> error $ "Identifier " ++ k ++ " already used") i x e

concatEnv :: Env a -> Env a -> Env a
concatEnv = M.unionWithKey (\k _ _ -> error $ "Duplicate values for " ++ k)

isInEnv :: Env a -> String -> Bool
isInEnv e i = M.member i e

enumEnv :: Env a -> [(String, a)]
enumEnv e = M.assocs e

removeFromEnv :: Env a -> String -> Env a
removeFromEnv e i = M.delete i e

removeFromEnvIf :: Env a -> (a -> Bool) -> Env a
removeFromEnvIf e f = M.mapMaybe (\x -> if f x then Nothing else Just x) e

mapEnv :: (a-> b) -> Env a -> Env b
mapEnv = M.map

mapEnvWithKey :: (String -> a -> b) -> Env a -> Env b
mapEnvWithKey = M.mapWithKey

printEnv :: PrettyPrint a => Env a -> IO ()
printEnv e = print $ map (\(k, v) -> (k, prettyPrint0 v)) (enumEnv e)

genVariable :: Env a -> String -> String
genVariable e = genVariableWith (isInEnv e)

genVariableWith :: (String -> Bool) -> String -> String
genVariableWith f s = if f s
    then genVariableWith f s'
    else s
    where
        s' = if isDigit c && c /= '9' then reverse $ c' : (tail $ reverse s) else s ++ "0"
        c = last s
        c' = chr (ord c + 1)
