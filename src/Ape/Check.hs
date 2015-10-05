module Ape.Check (check) where

import qualified Ape.Expr as E
import Ape.Print
import Ape.Env
import Ape.Type

import Control.Monad

type Check a = Either String a

enforce :: Type -> (Type -> Bool) -> String -> Check Type
enforce t f msg = if f t then Right t else Left msg

enforceInteger :: Type -> Check Type
enforceInteger t = enforce t isInteger $ "Integer expected, got \'" ++ (prettyPrint0 t) ++ "\'"
enforceTuple :: Type -> Check Type
enforceTuple t = enforce t isTuple $ "Tuple expected, got \'" ++ (prettyPrint0 t) ++ "\'"
enforceLambda :: Type -> Check Type
enforceLambda t = enforce t isLambda $ "Lambda expected, got \'" ++ (prettyPrint0 t) ++ "\'"
enforceNumeric :: Type -> Check Type
enforceNumeric t = enforce t isNumeric $ "Numeric type expected, got \'" ++ (prettyPrint0 t) ++ "\'"
enforceEqual :: Type -> Type -> Check Type
enforceEqual t t' = enforce t (==t') $ "Mismatching types, got \'" ++ (prettyPrint0 t) ++ "\' and \'" ++ (prettyPrint0 t') ++ "\'"

checkBinOp :: Env Type -> E.Value -> E.Value -> Either String Type
checkBinOp e a b = do
    ta <- check e a
    tb <- check e b
    enforceEqual ta tb

class Checkable a where
    check :: Env Type -> a -> Check Type

instance Checkable E.Value where
    check _ (E.I1  x) = return . I1  $ length x
    check _ (E.I8  x) = return . I8  $ length x
    check _ (E.I16 x) = return . I16 $ length x
    check _ (E.I32 x) = return . I32 $ length x
    check _ (E.I64 x) = return . I64 $ length x
    check _ (E.U8  x) = return . U8  $ length x
    check _ (E.U16 x) = return . U16 $ length x
    check _ (E.U32 x) = return . U32 $ length x
    check _ (E.U64 x) = return . U64 $ length x
    check _ (E.F32 x) = return . F32 $ length x
    check _ (E.F64 x) = return . F64 $ length x
    check e (E.Tuple v) = do
        tv <- mapM (check e) v
        return (Tuple tv)
    check e (E.Var v) = if isInEnv e v
        then Right $ lookupEnv e v
        else Left $ "Undeclared identifier " ++ v
    check e (E.Lambda v t b) = do
        tb <- check e' b
        return $ Lambda t tb
        where
            e' = insertEnv e v t

instance Checkable E.CExpr where
    check e (E.App (x:xs)) = do
        tx <- check e x
        foldM (\tr a -> do
            ta <- check e a
            _ <- enforceLambda tr
            _ <- enforceEqual (lambdaArg tr) ta
            return $ lambdaReturn tr) tx xs
    check _ (E.App []) = Left "Empty application"
    check e (E.If c t f) = do
        tc <- check e c
        tt <- check e t
        tf <- check e f
        _  <- enforceEqual tc (I1 1)
        enforceEqual tt tf
    check e (E.Atomic a) = check e a

instance Checkable E.AExpr where
    check e (E.PrimOp E.Add [a, b]) = checkBinOp e a b >>= enforceNumeric
    check e (E.PrimOp E.Sub [a, b]) = checkBinOp e a b >>= enforceNumeric
    check e (E.PrimOp E.Mul [a, b]) = checkBinOp e a b >>= enforceNumeric
    check e (E.PrimOp E.Div [a, b]) = checkBinOp e a b >>= enforceNumeric
    check e (E.PrimOp (E.Cmp E.Equal) [a, b]) = checkBinOp e a b >> return (I1 1)
    check e (E.PrimOp (E.Cmp _) [a, b]) = checkBinOp e a b >>= enforceNumeric >> return (I1 1)
    check e (E.PrimOp E.RShift [a, b]) = checkBinOp e a b >>= enforceInteger
    check e (E.PrimOp E.LShift [a, b]) = checkBinOp e a b >>= enforceInteger
    check e (E.PrimOp E.And [a, b]) = checkBinOp e a b >>= enforceInteger
    check e (E.PrimOp E.Or [a, b]) = checkBinOp e a b >>= enforceInteger
    check e (E.PrimOp E.Xor [a, b]) = checkBinOp e a b >>= enforceInteger
    check e (E.PrimOp E.Select [a, b, c]) = do
        ta <- check e a
        tb <- check e b
        tc <- check e c
        _ <- enforceEqual (vectorElement ta) (I1 1)
        _ <- enforce ta ((vectorSize tb ==) . vectorSize) "Vector sizes do not match in select"
        enforceEqual tb tc
    check e (E.PrimOp (E.BitCast t) [a]) = do
        ta <- check e a
        _ <- enforce t ((bitCount ta ==) . bitCount) "Sizes do not match in bitcast"
        enforce t (/= ta) "Cannot bitcast to the same type"
    check e (E.PrimOp (E.TupleElem i) [a]) = do
        ta <- check e a
        _ <- enforceTuple ta
        return $ tupleElement ta i
    check _ (E.PrimOp _ _) = Left "Invalid parameters in primitive operator"
    check e (E.Val v) = check e v

instance Checkable E.Expr where
    check e (E.Let v b) = do
        let e' = foldl (\f (i, t, _) -> insertEnv f i t) e v
        forM_ v (\(_, t, b') -> do
            t' <- check e' b'
            enforceEqual t t')
        check e' b
    check e (E.Complex c) = check e c
