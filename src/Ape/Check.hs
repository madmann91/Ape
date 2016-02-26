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

checkBinOp :: Env Type -> (Type -> Check Type) -> E.Info -> E.Op -> E.Value E.Info -> E.Value E.Info -> Check (E.AExpr E.Info)
checkBinOp e f i op a b = do
    ta <- check e a
    tb <- check e b
    let ta' = E.typeInfo $ E.info ta
    let tb' = E.typeInfo $ E.info tb
    _ <- enforceEqual ta' tb'
    t' <- f ta'
    return $ E.PrimOp (E.updateType i t') op [ta, tb]

class Checkable a where
    check :: E.Annotated a => Env Type -> a E.Info -> Check (a E.Info)

instance Checkable E.Value where
    check _ (E.I1  i x) = return $ E.I1  (E.updateType i $ I1  $ length x) x
    check _ (E.I8  i x) = return $ E.I8  (E.updateType i $ I8  $ length x) x
    check _ (E.I16 i x) = return $ E.I16 (E.updateType i $ I16 $ length x) x
    check _ (E.I32 i x) = return $ E.I32 (E.updateType i $ I32 $ length x) x
    check _ (E.I64 i x) = return $ E.I64 (E.updateType i $ I64 $ length x) x
    check _ (E.U8  i x) = return $ E.U8  (E.updateType i $ U8  $ length x) x
    check _ (E.U16 i x) = return $ E.U16 (E.updateType i $ U16 $ length x) x
    check _ (E.U32 i x) = return $ E.U32 (E.updateType i $ U32 $ length x) x
    check _ (E.U64 i x) = return $ E.U64 (E.updateType i $ U64 $ length x) x
    check _ (E.F32 i x) = return $ E.F32 (E.updateType i $ F32 $ length x) x
    check _ (E.F64 i x) = return $ E.F64 (E.updateType i $ F64 $ length x) x
    check e (E.Tuple i v) = do
        tv <- mapM (check e) v
        return $ E.Tuple (E.updateType i (Tuple $ map (E.typeInfo . E.info) tv)) tv
    check e (E.Var i v) = if isInEnv e v
        then Right $ E.Var (E.updateType i $ lookupEnv e v) v
        else Left $ "Undeclared identifier " ++ v
    check e (E.Lambda i v t b) = do
        tb <- check e' b
        return $ E.Lambda (E.updateType i $ Lambda t $ E.typeInfo $ E.info tb) v t tb
        where
            e' = insertEnv e v t

instance Checkable E.CExpr where
    check e (E.App i (x:xs)) = do
        tx <- check e x
        txs <- mapM (check e) xs
        t <- foldM (\tr ta -> do
            _ <- enforceLambda tr
            _ <- enforceEqual (lambdaArg tr) (E.typeInfo $ E.info ta)
            return $ lambdaReturn tr) (E.typeInfo $ E.info tx) txs
        return $ E.App (E.updateType i t) (tx:txs)
    check _ (E.App _ []) = Left "Empty application"
    check e (E.If i c t f) = do
        tc <- check e c
        tt <- check e t
        tf <- check e f
        _  <- enforceEqual (E.typeInfo $ E.info tc) (I1 1)
        ti <- enforceEqual (E.typeInfo $ E.info tt) (E.typeInfo $ E.info tf)
        return $ E.If (E.updateType i ti) tc tt tf
    check e (E.Atomic a) = check e a >>= (return . E.Atomic)

instance Checkable E.AExpr where
    check e (E.PrimOp i E.Add [a, b]) = checkBinOp e enforceNumeric i E.Add a b
    check e (E.PrimOp i E.Sub [a, b]) = checkBinOp e enforceNumeric i E.Sub a b
    check e (E.PrimOp i E.Mul [a, b]) = checkBinOp e enforceNumeric i E.Mul a b
    check e (E.PrimOp i E.Div [a, b]) = checkBinOp e enforceNumeric i E.Div a b
    check e (E.PrimOp i (E.Cmp op) [a, b])
        | op == E.Equal || op == E.NotEqual =
            checkBinOp e (\t -> do
                case t of
                    Tuple _ -> Left "Tuples are not comparable"
                    Lambda _ _ -> Left "Lambdas are not comparable"
                    _ -> return $ I1 $ vectorSize t) i (E.Cmp op) a b
        | otherwise =
            checkBinOp e (\t -> do
                _ <- enforceNumeric t
                return $ I1 $ vectorSize t) i (E.Cmp op) a b
    check e (E.PrimOp i E.RShift [a, b]) = checkBinOp e enforceInteger i E.RShift a b
    check e (E.PrimOp i E.LShift [a, b]) = checkBinOp e enforceInteger i E.LShift a b
    check e (E.PrimOp i E.And [a, b])    = checkBinOp e enforceInteger i E.And    a b
    check e (E.PrimOp i E.Or [a, b])     = checkBinOp e enforceInteger i E.Or     a b
    check e (E.PrimOp i E.Xor [a, b])    = checkBinOp e enforceInteger i E.Xor    a b
    check e (E.PrimOp i E.Select [a, b, c]) = do
        ta <- check e a
        tb <- check e b
        tc <- check e c
        let ta' = E.typeInfo $ E.info ta
        let tb' = E.typeInfo $ E.info tb
        let tc' = E.typeInfo $ E.info tc
        _ <- enforceEqual (vectorElement ta') (I1 1)
        _ <- enforce ta' ((vectorSize tb' ==) . vectorSize) "Vector sizes do not match in select"
        _ <- enforceEqual tb' tc'
        return $ E.PrimOp (E.updateType i tb') E.Select [ta, tb, tc]
    check e (E.PrimOp i (E.BitCast t) [a]) = do
        ta <- check e a
        let ta' = E.typeInfo $ E.info ta
        _ <- enforce t ((bitCount ta' ==) . bitCount) "Sizes do not match in bitcast"
        return $ E.PrimOp (E.updateType i ta') (E.BitCast t) [ta]
    check e (E.PrimOp i (E.TupleElem j) [a]) = do
        ta <- check e a
        let ta' = E.typeInfo $ E.info ta
        _ <- enforceTuple ta'
        return $ E.PrimOp (E.updateType i $ tupleElement ta' j) (E.TupleElem j) [ta]
    check _ (E.PrimOp _ _ _) = Left "Invalid parameters in primitive operator"
    check e (E.Val v) = check e v >>= (return . E.Val)

instance Checkable E.Expr where
    check e (E.Let i v b) = do
        let e' = foldl (\f (w, t, _) -> insertEnv f w t) e v
        tv <- mapM (\(w, t, b') -> do
            tb' <- check e' b'
            _ <- enforceEqual t $ E.typeInfo $ E.info tb'
            return (w, t, tb')) v
        tb <- check e' b
        return $ E.Let (E.updateType i $ E.typeInfo $ E.info tb) tv tb
    check e (E.Complex c) = check e c >>= (return . E.Complex)
