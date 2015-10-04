module Ape.Type where

data Type = I1 Int
          | I8 Int
          | I16 Int
          | I32 Int
          | I64 Int
          | U8 Int
          | U16 Int
          | U32 Int
          | U64 Int
          | F32 Int
          | F64 Int
          | Tuple [Type]
          | Lambda Type Type
          deriving (Eq, Ord)

typeSize :: Type -> Int
typeSize (Tuple t) = 1 + (sum $ map typeSize t)
typeSize (Lambda a r) = 1 + typeSize a + typeSize r
typeSize _ = 1

bitCount :: Type -> Int
bitCount (I1  n) = n * 1
bitCount (I8  n) = n * 8
bitCount (I16 n) = n * 16
bitCount (I32 n) = n * 32
bitCount (I64 n) = n * 64
bitCount (U8  n) = n * 8
bitCount (U16 n) = n * 16
bitCount (U32 n) = n * 32
bitCount (U64 n) = n * 64
bitCount (F32 n) = n * 32
bitCount (F64 n) = n * 64
bitCount (Tuple t) = sum (map bitCount t)
bitCount _ = error "Invalid type for bitcount"

isInteger :: Type -> Bool
isInteger (I1  _) = True
isInteger (I8  _) = True
isInteger (I16 _) = True
isInteger (I32 _) = True
isInteger (I64 _) = True
isInteger (U8  _) = True
isInteger (U16 _) = True
isInteger (U32 _) = True
isInteger (U64 _) = True
isInteger _ = False

isNumeric :: Type -> Bool
isNumeric (I8  _) = True
isNumeric (I16 _) = True
isNumeric (I32 _) = True
isNumeric (I64 _) = True
isNumeric (U8  _) = True
isNumeric (U16 _) = True
isNumeric (U32 _) = True
isNumeric (U64 _) = True
isNumeric (F32 _) = True
isNumeric (F64 _) = True
isNumeric _ = False

isVector :: Type -> Bool
isVector (I1  n) = n > 1
isVector (I8  n) = n > 1
isVector (I16 n) = n > 1
isVector (I32 n) = n > 1
isVector (I64 n) = n > 1
isVector (U8  n) = n > 1
isVector (U16 n) = n > 1
isVector (U32 n) = n > 1
isVector (U64 n) = n > 1
isVector (F32 n) = n > 1
isVector (F64 n) = n > 1
isVector _ = False

isTuple :: Type -> Bool
isTuple (Tuple _) = True
isTuple _ = False

isLambda :: Type -> Bool
isLambda (Lambda _ _) = True
isLambda _ = False

tupleElement :: Type -> Int -> Type
tupleElement (Tuple t) i = t !! i
tupleElement _ _ = error "Not a tuple type"

tupleSize :: Type -> Int
tupleSize (Tuple t) = length t
tupleSize _ = error "Not a tuple type"

vectorElement :: Type -> Type
vectorElement (I1  _) = I1  1
vectorElement (I8  _) = I8  1
vectorElement (I16 _) = I16 1
vectorElement (I32 _) = I32 1
vectorElement (I64 _) = I64 1
vectorElement (U8  _) = U8  1
vectorElement (U16 _) = U16 1
vectorElement (U32 _) = U32 1
vectorElement (U64 _) = U64 1
vectorElement (F32 _) = F32 1
vectorElement (F64 _) = F64 1
vectorElement _ = error "Not a vector type"

vectorSize :: Type -> Int
vectorSize (I1  n) = n
vectorSize (I8  n) = n
vectorSize (I16 n) = n
vectorSize (I32 n) = n
vectorSize (I64 n) = n
vectorSize (U8  n) = n
vectorSize (U16 n) = n
vectorSize (U32 n) = n
vectorSize (U64 n) = n
vectorSize (F32 n) = n
vectorSize (F64 n) = n
vectorSize _ = error "Not a vector type"

lambdaArg :: Type -> Type
lambdaArg (Lambda a _) = a
lambdaArg _ = error "Not a lambda type"

lambdaReturn :: Type -> Type
lambdaReturn (Lambda _ r) = r
lambdaReturn _ = error "Not a lambda type"
