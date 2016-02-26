module Ape.Eval (eval) where

import qualified Ape.Type as T
import Ape.Expr
import Ape.Print

import Data.Bits
import Unsafe.Coerce

binOpError :: String -> Value a -> Value a -> b
binOpError str a b = error $ str ++ ": " ++ prettyPrint0 a ++ ", " ++ prettyPrint0 b

addValues :: a -> Value a -> Value a -> Value a
addValues i (I8  _ a) (I8  _ b) = I8  i $ zipWith (+) a b
addValues i (I16 _ a) (I16 _ b) = I16 i $ zipWith (+) a b
addValues i (I32 _ a) (I32 _ b) = I32 i $ zipWith (+) a b
addValues i (I64 _ a) (I64 _ b) = I64 i $ zipWith (+) a b
addValues i (U8  _ a) (U8  _ b) = U8  i $ zipWith (+) a b
addValues i (U16 _ a) (U16 _ b) = U16 i $ zipWith (+) a b
addValues i (U32 _ a) (U32 _ b) = U32 i $ zipWith (+) a b
addValues i (U64 _ a) (U64 _ b) = U64 i $ zipWith (+) a b
addValues i (F32 _ a) (F32 _ b) = F32 i $ zipWith (+) a b
addValues i (F64 _ a) (F64 _ b) = F64 i $ zipWith (+) a b
addValues _ a b = binOpError "Invalid operands for addition" a b

subValues :: a -> Value a -> Value a -> Value a
subValues i (I8  _ a) (I8  _ b) = I8  i $ zipWith (-) a b
subValues i (I16 _ a) (I16 _ b) = I16 i $ zipWith (-) a b
subValues i (I32 _ a) (I32 _ b) = I32 i $ zipWith (-) a b
subValues i (I64 _ a) (I64 _ b) = I64 i $ zipWith (-) a b
subValues i (U8  _ a) (U8  _ b) = U8  i $ zipWith (-) a b
subValues i (U16 _ a) (U16 _ b) = U16 i $ zipWith (-) a b
subValues i (U32 _ a) (U32 _ b) = U32 i $ zipWith (-) a b
subValues i (U64 _ a) (U64 _ b) = U64 i $ zipWith (-) a b
subValues i (F32 _ a) (F32 _ b) = F32 i $ zipWith (-) a b
subValues i (F64 _ a) (F64 _ b) = F64 i $ zipWith (-) a b
subValues _ a b = binOpError "Invalid operands for subtraction" a b

mulValues :: a -> Value a -> Value a -> Value a
mulValues i (I8  _ a) (I8  _ b) = I8  i $ zipWith (*) a b
mulValues i (I16 _ a) (I16 _ b) = I16 i $ zipWith (*) a b
mulValues i (I32 _ a) (I32 _ b) = I32 i $ zipWith (*) a b
mulValues i (I64 _ a) (I64 _ b) = I64 i $ zipWith (*) a b
mulValues i (U8  _ a) (U8  _ b) = U8  i $ zipWith (*) a b
mulValues i (U16 _ a) (U16 _ b) = U16 i $ zipWith (*) a b
mulValues i (U32 _ a) (U32 _ b) = U32 i $ zipWith (*) a b
mulValues i (U64 _ a) (U64 _ b) = U64 i $ zipWith (*) a b
mulValues i (F32 _ a) (F32 _ b) = F32 i $ zipWith (*) a b
mulValues i (F64 _ a) (F64 _ b) = F64 i $ zipWith (*) a b
mulValues _ a b = binOpError "Invalid operands for multiplication" a b

divValues :: a -> Value a -> Value a -> Value a
divValues i (I8  _ a) (I8  _ b) = I8  i $ zipWith div a b
divValues i (I16 _ a) (I16 _ b) = I16 i $ zipWith div a b
divValues i (I32 _ a) (I32 _ b) = I32 i $ zipWith div a b
divValues i (I64 _ a) (I64 _ b) = I64 i $ zipWith div a b
divValues i (U8  _ a) (U8  _ b) = U8  i $ zipWith div a b
divValues i (U16 _ a) (U16 _ b) = U16 i $ zipWith div a b
divValues i (U32 _ a) (U32 _ b) = U32 i $ zipWith div a b
divValues i (U64 _ a) (U64 _ b) = U64 i $ zipWith div a b
divValues i (F32 _ a) (F32 _ b) = F32 i $ zipWith (/) a b
divValues i (F64 _ a) (F64 _ b) = F64 i $ zipWith (/) a b
divValues _ a b = binOpError "Invalid operands for division" a b

leftShiftValues :: a -> Value a -> Value a -> Value a
leftShiftValues i (I8  _ a) (I8  _ b) = I8  i $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues i (I16 _ a) (I16 _ b) = I16 i $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues i (I32 _ a) (I32 _ b) = I32 i $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues i (I64 _ a) (I64 _ b) = I64 i $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues i (U8  _ a) (U8  _ b) = U8  i $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues i (U16 _ a) (U16 _ b) = U16 i $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues i (U32 _ a) (U32 _ b) = U32 i $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues i (U64 _ a) (U64 _ b) = U64 i $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues _ a b = binOpError "Invalid operands for left shift" a b

rightShiftValues :: a -> Value a -> Value a -> Value a
rightShiftValues i (I8  _ a) (I8  _ b) = I8  i $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues i (I16 _ a) (I16 _ b) = I16 i $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues i (I32 _ a) (I32 _ b) = I32 i $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues i (I64 _ a) (I64 _ b) = I64 i $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues i (U8  _ a) (U8  _ b) = U8  i $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues i (U16 _ a) (U16 _ b) = U16 i $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues i (U32 _ a) (U32 _ b) = U32 i $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues i (U64 _ a) (U64 _ b) = U64 i $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues _ a b = binOpError "Invalid operands for right shift" a b

andValues :: a -> Value a -> Value a -> Value a
andValues i (I1  _ a) (I1  _ b) = I1  i $ zipWith (.&.) a b
andValues i (I8  _ a) (I8  _ b) = I8  i $ zipWith (.&.) a b
andValues i (I16 _ a) (I16 _ b) = I16 i $ zipWith (.&.) a b
andValues i (I32 _ a) (I32 _ b) = I32 i $ zipWith (.&.) a b
andValues i (I64 _ a) (I64 _ b) = I64 i $ zipWith (.&.) a b
andValues i (U8  _ a) (U8  _ b) = U8  i $ zipWith (.&.) a b
andValues i (U16 _ a) (U16 _ b) = U16 i $ zipWith (.&.) a b
andValues i (U32 _ a) (U32 _ b) = U32 i $ zipWith (.&.) a b
andValues i (U64 _ a) (U64 _ b) = U64 i $ zipWith (.&.) a b
andValues _ a b = binOpError "Invalid operands for bitwise and" a b

orValues :: a -> Value a -> Value a -> Value a
orValues i (I1  _ a) (I1  _ b) = I1  i $ zipWith (.|.) a b
orValues i (I8  _ a) (I8  _ b) = I8  i $ zipWith (.|.) a b
orValues i (I16 _ a) (I16 _ b) = I16 i $ zipWith (.|.) a b
orValues i (I32 _ a) (I32 _ b) = I32 i $ zipWith (.|.) a b
orValues i (I64 _ a) (I64 _ b) = I64 i $ zipWith (.|.) a b
orValues i (U8  _ a) (U8  _ b) = U8  i $ zipWith (.|.) a b
orValues i (U16 _ a) (U16 _ b) = U16 i $ zipWith (.|.) a b
orValues i (U32 _ a) (U32 _ b) = U32 i $ zipWith (.|.) a b
orValues i (U64 _ a) (U64 _ b) = U64 i $ zipWith (.|.) a b
orValues _ a b = binOpError "Invalid operands for bitwise or" a b

xorValues :: a -> Value a -> Value a -> Value a
xorValues i (I1  _ a) (I1  _ b) = I1  i $ zipWith xor a b
xorValues i (I8  _ a) (I8  _ b) = I8  i $ zipWith xor a b
xorValues i (I16 _ a) (I16 _ b) = I16 i $ zipWith xor a b
xorValues i (I32 _ a) (I32 _ b) = I32 i $ zipWith xor a b
xorValues i (I64 _ a) (I64 _ b) = I64 i $ zipWith xor a b
xorValues i (U8  _ a) (U8  _ b) = U8  i $ zipWith xor a b
xorValues i (U16 _ a) (U16 _ b) = U16 i $ zipWith xor a b
xorValues i (U32 _ a) (U32 _ b) = U32 i $ zipWith xor a b
xorValues i (U64 _ a) (U64 _ b) = U64 i $ zipWith xor a b
xorValues _ a b = error "Invalid operands for bitwise ior" a b

select :: Bool -> a -> a -> a
select a b c = if a then b else c

selectValues :: a -> Value a -> Value a -> Value a -> Value a
selectValues i (I1 _ a) (I1  _ b) (I1  _ c) = I1  i $ zipWith3 select a b c
selectValues i (I1 _ a) (I8  _ b) (I8  _ c) = I8  i $ zipWith3 select a b c
selectValues i (I1 _ a) (I16 _ b) (I16 _ c) = I16 i $ zipWith3 select a b c
selectValues i (I1 _ a) (I32 _ b) (I32 _ c) = I32 i $ zipWith3 select a b c
selectValues i (I1 _ a) (I64 _ b) (I64 _ c) = I64 i $ zipWith3 select a b c
selectValues i (I1 _ a) (U8  _ b) (U8  _ c) = U8  i $ zipWith3 select a b c
selectValues i (I1 _ a) (U16 _ b) (U16 _ c) = U16 i $ zipWith3 select a b c
selectValues i (I1 _ a) (U32 _ b) (U32 _ c) = U32 i $ zipWith3 select a b c
selectValues i (I1 _ a) (U64 _ b) (U64 _ c) = U64 i $ zipWith3 select a b c
selectValues _ _ _ _ = error "Invalid operands for select"

compareFunction :: (Ord a) => CmpOp -> (a -> a -> Bool)
compareFunction Equal        = (==)
compareFunction NotEqual     = (/=)
compareFunction Greater      = (>)
compareFunction Less         = (<)
compareFunction GreaterEqual = (>=)
compareFunction LessEqual    = (<=)

compareValues :: a -> CmpOp -> Value a -> Value a -> Value a
compareValues i op (I8  _ a) (I8  _ b) = I1 i $ zipWith (compareFunction op) a b
compareValues i op (I16 _ a) (I16 _ b) = I1 i $ zipWith (compareFunction op) a b
compareValues i op (I32 _ a) (I32 _ b) = I1 i $ zipWith (compareFunction op) a b
compareValues i op (I64 _ a) (I64 _ b) = I1 i $ zipWith (compareFunction op) a b
compareValues i op (U8  _ a) (U8  _ b) = I1 i $ zipWith (compareFunction op) a b
compareValues i op (U16 _ a) (U16 _ b) = I1 i $ zipWith (compareFunction op) a b
compareValues i op (U32 _ a) (U32 _ b) = I1 i $ zipWith (compareFunction op) a b
compareValues i op (U64 _ a) (U64 _ b) = I1 i $ zipWith (compareFunction op) a b
compareValues i op (F32 _ a) (F32 _ b) = I1 i $ zipWith (compareFunction op) a b
compareValues i op (F64 _ a) (F64 _ b) = I1 i $ zipWith (compareFunction op) a b
compareValues _ _ _ _ = error "Invalid operands for comparison"

bitcastValue :: a -> T.Type -> Value a -> Value a
bitcastValue i (T.U8  _) (I8  _ a) = U8  i $ map unsafeCoerce a
bitcastValue i (T.I8  _) (U8  _ a) = I8  i $ map unsafeCoerce a
bitcastValue i (T.U16 _) (I16 _ a) = U16 i $ map unsafeCoerce a
bitcastValue i (T.I16 _) (U16 _ a) = I16 i $ map unsafeCoerce a
bitcastValue i (T.U32 _) (I32 _ a) = U32 i $ map unsafeCoerce a
bitcastValue i (T.I32 _) (U32 _ a) = I32 i $ map unsafeCoerce a
bitcastValue i (T.I32 _) (F32 _ a) = I32 i $ map unsafeCoerce a
bitcastValue i (T.F32 _) (I32 _ a) = F32 i $ map unsafeCoerce a
bitcastValue i (T.U32 _) (F32 _ a) = U32 i $ map unsafeCoerce a
bitcastValue i (T.F32 _) (U32 _ a) = F32 i $ map unsafeCoerce a
bitcastValue i (T.U64 _) (I64 _ a) = U64 i $ map unsafeCoerce a
bitcastValue i (T.I64 _) (U64 _ a) = I64 i $ map unsafeCoerce a
bitcastValue i (T.I64 _) (F64 _ a) = I64 i $ map unsafeCoerce a
bitcastValue i (T.F64 _) (I64 _ a) = F64 i $ map unsafeCoerce a
bitcastValue i (T.U64 _) (F64 _ a) = U64 i $ map unsafeCoerce a
bitcastValue i (T.F64 _) (U64 _ a) = F64 i $ map unsafeCoerce a
bitcastValue _ _ _ = error "Invalid operands for bitcast"

tupleElem :: a -> Int -> Value a -> Value a
tupleElem i j (Tuple _ e) = updateInfo (e !! j) i
tupleElem _ _ _ = error "Invalid operands for tuple element operator"

vectorElem :: a -> Int -> Value a -> Value a
vectorElem i j (I1  _ e) = I1  i [e !! j]
vectorElem i j (I8  _ e) = I8  i [e !! j]
vectorElem i j (I16 _ e) = I16 i [e !! j]
vectorElem i j (I32 _ e) = I32 i [e !! j]
vectorElem i j (I64 _ e) = I64 i [e !! j]
vectorElem i j (U8  _ e) = U8  i [e !! j]
vectorElem i j (U16 _ e) = U16 i [e !! j]
vectorElem i j (U32 _ e) = U32 i [e !! j]
vectorElem i j (U64 _ e) = U64 i [e !! j]
vectorElem i j (F32 _ e) = F32 i [e !! j]
vectorElem i j (F64 _ e) = F64 i [e !! j]
vectorElem _ _ _ = error "Invalid operands for vector element operator"

eval :: a -> Op -> [Value a] -> Value a
eval i Add [a, b] = addValues i a b
eval i Sub [a, b] = subValues i a b
eval i Mul [a, b] = mulValues i a b
eval i Div [a, b] = divValues i a b
eval i LShift [a, b] = leftShiftValues i a b
eval i RShift [a, b] = rightShiftValues i a b
eval i And [a, b] = andValues i a b
eval i Or [a, b] = orValues i a b
eval i Xor [a, b] = xorValues i a b
eval i Select [a, b, c] = selectValues i a b c
eval i (Cmp op) [a, b] = compareValues i op a b
eval i (BitCast t) [a] = bitcastValue i t a
eval i (TupleElem j) [a] = tupleElem i j a
eval i (VectorElem j) [a] = vectorElem i j a
eval _ _ _ = error "Invalid operands for primop"
