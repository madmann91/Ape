module Ape.Eval (eval) where

import qualified Ape.Type as T
import Ape.Expr

import Data.Bits
import Unsafe.Coerce

addValues :: Value -> Value -> Value
addValues (I8  a) (I8  b) = I8  $ zipWith (+) a b
addValues (I16 a) (I16 b) = I16 $ zipWith (+) a b
addValues (I32 a) (I32 b) = I32 $ zipWith (+) a b
addValues (I64 a) (I64 b) = I64 $ zipWith (+) a b
addValues (U8  a) (U8  b) = U8  $ zipWith (+) a b
addValues (U16 a) (U16 b) = U16 $ zipWith (+) a b
addValues (U32 a) (U32 b) = U32 $ zipWith (+) a b
addValues (U64 a) (U64 b) = U64 $ zipWith (+) a b
addValues (F32 a) (F32 b) = F32 $ zipWith (+) a b
addValues (F64 a) (F64 b) = F64 $ zipWith (+) a b
addValues _ _ = error "Invalid operands for addition"

subValues :: Value -> Value -> Value
subValues (I8  a) (I8  b) = I8  $ zipWith (-) a b
subValues (I16 a) (I16 b) = I16 $ zipWith (-) a b
subValues (I32 a) (I32 b) = I32 $ zipWith (-) a b
subValues (I64 a) (I64 b) = I64 $ zipWith (-) a b
subValues (U8  a) (U8  b) = U8  $ zipWith (-) a b
subValues (U16 a) (U16 b) = U16 $ zipWith (-) a b
subValues (U32 a) (U32 b) = U32 $ zipWith (-) a b
subValues (U64 a) (U64 b) = U64 $ zipWith (-) a b
subValues (F32 a) (F32 b) = F32 $ zipWith (-) a b
subValues (F64 a) (F64 b) = F64 $ zipWith (-) a b
subValues _ _ = error "Invalid operands for subtraction"

mulValues :: Value -> Value -> Value
mulValues (I8  a) (I8  b) = I8  $ zipWith (*) a b
mulValues (I16 a) (I16 b) = I16 $ zipWith (*) a b
mulValues (I32 a) (I32 b) = I32 $ zipWith (*) a b
mulValues (I64 a) (I64 b) = I64 $ zipWith (*) a b
mulValues (U8  a) (U8  b) = U8  $ zipWith (*) a b
mulValues (U16 a) (U16 b) = U16 $ zipWith (*) a b
mulValues (U32 a) (U32 b) = U32 $ zipWith (*) a b
mulValues (U64 a) (U64 b) = U64 $ zipWith (*) a b
mulValues (F32 a) (F32 b) = F32 $ zipWith (*) a b
mulValues (F64 a) (F64 b) = F64 $ zipWith (*) a b
mulValues _ _ = error "Invalid operands for multiplication"

divValues :: Value -> Value -> Value
divValues (I8  a) (I8  b) = I8  $ zipWith div a b
divValues (I16 a) (I16 b) = I16 $ zipWith div a b
divValues (I32 a) (I32 b) = I32 $ zipWith div a b
divValues (I64 a) (I64 b) = I64 $ zipWith div a b
divValues (U8  a) (U8  b) = U8  $ zipWith div a b
divValues (U16 a) (U16 b) = U16 $ zipWith div a b
divValues (U32 a) (U32 b) = U32 $ zipWith div a b
divValues (U64 a) (U64 b) = U64 $ zipWith div a b
divValues (F32 a) (F32 b) = F32 $ zipWith (/) a b
divValues (F64 a) (F64 b) = F64 $ zipWith (/) a b
divValues _ _ = error "Invalid operands for division"

leftShiftValues :: Value -> Value -> Value
leftShiftValues (I8  a) (I8  b) = I8  $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues (I16 a) (I16 b) = I16 $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues (I32 a) (I32 b) = I32 $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues (I64 a) (I64 b) = I64 $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues (U8  a) (U8  b) = U8  $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues (U16 a) (U16 b) = U16 $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues (U32 a) (U32 b) = U32 $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues (U64 a) (U64 b) = U64 $ zipWith (\x y -> shiftL x $ fromIntegral y) a b
leftShiftValues _ _ = error "Invalid operands for left shift"

rightShiftValues :: Value -> Value -> Value
rightShiftValues (I8  a) (I8  b) = I8  $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues (I16 a) (I16 b) = I16 $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues (I32 a) (I32 b) = I32 $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues (I64 a) (I64 b) = I64 $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues (U8  a) (U8  b) = U8  $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues (U16 a) (U16 b) = U16 $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues (U32 a) (U32 b) = U32 $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues (U64 a) (U64 b) = U64 $ zipWith (\x y -> shiftR x $ fromIntegral y) a b
rightShiftValues _ _ = error "Invalid operands for right shift"

andValues :: Value -> Value -> Value
andValues (I1  a) (I1  b) = I1  $ zipWith (.&.) a b
andValues (I8  a) (I8  b) = I8  $ zipWith (.&.) a b
andValues (I16 a) (I16 b) = I16 $ zipWith (.&.) a b
andValues (I32 a) (I32 b) = I32 $ zipWith (.&.) a b
andValues (I64 a) (I64 b) = I64 $ zipWith (.&.) a b
andValues (U8  a) (U8  b) = U8  $ zipWith (.&.) a b
andValues (U16 a) (U16 b) = U16 $ zipWith (.&.) a b
andValues (U32 a) (U32 b) = U32 $ zipWith (.&.) a b
andValues (U64 a) (U64 b) = U64 $ zipWith (.&.) a b
andValues _ _ = error "Invalid operands for bitwise and"

orValues :: Value -> Value -> Value
orValues (I1  a) (I1  b) = I1  $ zipWith (.|.) a b
orValues (I8  a) (I8  b) = I8  $ zipWith (.|.) a b
orValues (I16 a) (I16 b) = I16 $ zipWith (.|.) a b
orValues (I32 a) (I32 b) = I32 $ zipWith (.|.) a b
orValues (I64 a) (I64 b) = I64 $ zipWith (.|.) a b
orValues (U8  a) (U8  b) = U8  $ zipWith (.|.) a b
orValues (U16 a) (U16 b) = U16 $ zipWith (.|.) a b
orValues (U32 a) (U32 b) = U32 $ zipWith (.|.) a b
orValues (U64 a) (U64 b) = U64 $ zipWith (.|.) a b
orValues _ _ = error "Invalid operands for bitwise or"

xorValues :: Value -> Value -> Value
xorValues (I1  a) (I1  b) = I1  $ zipWith xor a b
xorValues (I8  a) (I8  b) = I8  $ zipWith xor a b
xorValues (I16 a) (I16 b) = I16 $ zipWith xor a b
xorValues (I32 a) (I32 b) = I32 $ zipWith xor a b
xorValues (I64 a) (I64 b) = I64 $ zipWith xor a b
xorValues (U8  a) (U8  b) = U8  $ zipWith xor a b
xorValues (U16 a) (U16 b) = U16 $ zipWith xor a b
xorValues (U32 a) (U32 b) = U32 $ zipWith xor a b
xorValues (U64 a) (U64 b) = U64 $ zipWith xor a b
xorValues _ _ = error "Invalid operands for bitwise xor"

select :: Bool -> a -> a -> a
select a b c = if a then b else c

selectValues :: Value -> Value -> Value-> Value
selectValues (I1 a) (I1  b) (I1  c) = I1  $ zipWith3 select a b c
selectValues (I1 a) (I8  b) (I8  c) = I8  $ zipWith3 select a b c
selectValues (I1 a) (I16 b) (I16 c) = I16 $ zipWith3 select a b c
selectValues (I1 a) (I32 b) (I32 c) = I32 $ zipWith3 select a b c
selectValues (I1 a) (I64 b) (I64 c) = I64 $ zipWith3 select a b c
selectValues (I1 a) (U8  b) (U8  c) = U8  $ zipWith3 select a b c
selectValues (I1 a) (U16 b) (U16 c) = U16 $ zipWith3 select a b c
selectValues (I1 a) (U32 b) (U32 c) = U32 $ zipWith3 select a b c
selectValues (I1 a) (U64 b) (U64 c) = U64 $ zipWith3 select a b c
selectValues _ _ _ = error "Invalid operands for select"

compareFunction :: (Ord a) => CmpOp -> (a -> a -> Bool)
compareFunction Equal        = (==)
compareFunction Greater      = (>)
compareFunction Less         = (<)
compareFunction GreaterEqual = (>=)
compareFunction LessEqual    = (<=)

compareValues :: CmpOp -> Value -> Value -> Value
compareValues op (I8  a) (I8  b) = I1 $ zipWith (compareFunction op) a b
compareValues op (I16 a) (I16 b) = I1 $ zipWith (compareFunction op) a b
compareValues op (I32 a) (I32 b) = I1 $ zipWith (compareFunction op) a b
compareValues op (I64 a) (I64 b) = I1 $ zipWith (compareFunction op) a b
compareValues op (U8  a) (U8  b) = I1 $ zipWith (compareFunction op) a b
compareValues op (U16 a) (U16 b) = I1 $ zipWith (compareFunction op) a b
compareValues op (U32 a) (U32 b) = I1 $ zipWith (compareFunction op) a b
compareValues op (U64 a) (U64 b) = I1 $ zipWith (compareFunction op) a b
compareValues op (F32 a) (F32 b) = I1 $ zipWith (compareFunction op) a b
compareValues op (F64 a) (F64 b) = I1 $ zipWith (compareFunction op) a b
compareValues _ _ _ = error "Invalid operands for comparison"

bitcastValue :: T.Type -> Value -> Value
bitcastValue (T.I1 _) x@(I1 _) = x
bitcastValue (T.I8 _) x@(I8 _) = x
bitcastValue (T.U8 _) x@(U8 _) = x
bitcastValue (T.U8 _) (I8 x) = U8 $ map unsafeCoerce x
bitcastValue (T.I8 _) (U8 x) = I8 $ map unsafeCoerce x
bitcastValue (T.I16 _) x@(I16 _) = x
bitcastValue (T.U16 _) x@(U16 _) = x
bitcastValue (T.U16 _) (I16 x) = U16 $ map unsafeCoerce x
bitcastValue (T.I16 _) (U16 x) = I16 $ map unsafeCoerce x
bitcastValue (T.I32 _) x@(I32 _) = x
bitcastValue (T.U32 _) x@(U32 _) = x
bitcastValue (T.F32 _) x@(F32 _) = x
bitcastValue (T.U32 _) (I32 x) = U32 $ map unsafeCoerce x
bitcastValue (T.I32 _) (U32 x) = I32 $ map unsafeCoerce x
bitcastValue (T.I32 _) (F32 x) = I32 $ map unsafeCoerce x
bitcastValue (T.F32 _) (I32 x) = F32 $ map unsafeCoerce x
bitcastValue (T.U32 _) (F32 x) = U32 $ map unsafeCoerce x
bitcastValue (T.F32 _) (U32 x) = F32 $ map unsafeCoerce x
bitcastValue (T.I64 _) x@(I64 _) = x
bitcastValue (T.U64 _) x@(U64 _) = x
bitcastValue (T.F64 _) x@(F64 _) = x
bitcastValue (T.U64 _) (I64 x) = U64 $ map unsafeCoerce x
bitcastValue (T.I64 _) (U64 x) = I64 $ map unsafeCoerce x
bitcastValue (T.I64 _) (F64 x) = I64 $ map unsafeCoerce x
bitcastValue (T.F64 _) (I64 x) = F64 $ map unsafeCoerce x
bitcastValue (T.U64 _) (F64 x) = U64 $ map unsafeCoerce x
bitcastValue (T.F64 _) (U64 x) = F64 $ map unsafeCoerce x
bitcastValue _ _ = error "Invalid operands for bitcast"

tupleElem :: Int -> Value -> Value
tupleElem i (Tuple e) = e !! i
tupleElem _ _ = error "Invalid operands for tuple element operator"

vectorElem :: Int -> Value -> Value
vectorElem i (I1  e) = I1  [e !! i]
vectorElem i (I8  e) = I8  [e !! i]
vectorElem i (I16 e) = I16 [e !! i]
vectorElem i (I32 e) = I32 [e !! i]
vectorElem i (I64 e) = I64 [e !! i]
vectorElem i (U8  e) = U8  [e !! i]
vectorElem i (U16 e) = U16 [e !! i]
vectorElem i (U32 e) = U32 [e !! i]
vectorElem i (U64 e) = U64 [e !! i]
vectorElem i (F32 e) = F32 [e !! i]
vectorElem i (F64 e) = F64 [e !! i]
vectorElem _ _ = error "Invalid operands for vector element operator"

eval :: Op -> [Value] -> Value
eval Add [a, b] = addValues a b
eval Sub [a, b] = subValues a b
eval Mul [a, b] = mulValues a b
eval Div [a, b] = divValues a b
eval LShift [a, b] = leftShiftValues a b
eval RShift [a, b] = rightShiftValues a b
eval And [a, b] = andValues a b
eval Or [a, b] = orValues a b
eval Xor [a, b] = xorValues a b
eval Select [a, b, c] = selectValues a b c
eval (Cmp op) [a, b] = (compareValues op) a b
eval (BitCast t) [a] = bitcastValue t a
eval (TupleElem i) [a] = tupleElem i a
eval (VectorElem i) [a] = vectorElem i a
eval _ _ = error "Invalid operands for primop"
