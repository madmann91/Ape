module Ape.Print where

import Ape.Expr as E
import Ape.Type as T

import Data.List

-- Contains the indentation level
newtype Printer = Printer Int

class PrettyPrint a where
    prettyPrint :: Printer -> a -> String
    prettyPrint0 :: a -> String
    prettyPrint (Printer _) v = (prettyPrint0 v)
    prettyPrint0 = prettyPrint (Printer 0)

splitLineLimit :: Int
splitLineLimit = 10

printList0 :: (PrettyPrint a) => String -> [a] -> String
printList0 = printList (Printer 0)

printList :: (PrettyPrint a) => Printer -> String -> [a] -> String
printList p sep xs = concat (intersperse sep $ map (prettyPrint p) xs)

showVector :: (Show a) => [a] -> String
showVector [x] = show x
showVector xs = "<" ++ (concat (intersperse ", " $ map show xs)) ++ ">"

indent :: Int -> String
indent i = replicate (i * 2) ' '

printVectorSize :: Int -> String
printVectorSize i = if i == 1 then "" else "<" ++ (show i) ++ ">"

printBinOp :: E.Value a -> E.Value a -> String -> String
printBinOp a b op = (prettyPrint0 a) ++ " " ++ op ++ " " ++ (prettyPrint0 b)

instance PrettyPrint (E.Value a) where
    prettyPrint0 (E.I1 _ v) = "i1 "  ++ printBoolVector v
        where
            printBool x = if x then "true" else "false"
            printBoolVector [x] = printBool x
            printBoolVector xs = "<" ++ (concat (intersperse ", " $ map printBool xs)) ++ ">"
    prettyPrint0 (E.I8  _ v) = "i8 "  ++ showVector v
    prettyPrint0 (E.I16 _ v) = "i16 " ++ showVector v
    prettyPrint0 (E.I32 _ v) = "i32 " ++ showVector v
    prettyPrint0 (E.I64 _ v) = "i64 " ++ showVector v
    prettyPrint0 (E.U8  _ v) = "u8 "  ++ showVector v
    prettyPrint0 (E.U16 _ v) = "u16 " ++ showVector v
    prettyPrint0 (E.U32 _ v) = "u32 " ++ showVector v
    prettyPrint0 (E.U64 _ v) = "u64 " ++ showVector v
    prettyPrint0 (E.F32 _ v) = "f32 " ++ showVector v
    prettyPrint0 (E.F64 _ v) = "f64 " ++ showVector v
    prettyPrint0 (E.Tuple _ v) = "(" ++ (printList0 ", " v) ++ ")"
    prettyPrint0 (E.Var _ v) = v
    prettyPrint0 l@(E.Lambda _ _ _ _) = prettyPrint (Printer 0) l
    prettyPrint (Printer ind) (E.Lambda _ v t b) = "lambda " ++ arg ++ sep ++ body
        where
            arg = v ++ ": " ++ prettyPrint0 t
            body = prettyPrint (Printer $ ind + 1) b
            sep = if exprSize b > splitLineLimit then "\n" ++ (indent $ ind + 1) else " "
    prettyPrint (Printer _) v = prettyPrint0 v

instance PrettyPrint T.Type where
    prettyPrint0 (T.I1  i) = "i1"  ++ printVectorSize i
    prettyPrint0 (T.I8  i) = "i8"  ++ printVectorSize i
    prettyPrint0 (T.I16 i) = "i16" ++ printVectorSize i
    prettyPrint0 (T.I32 i) = "i32" ++ printVectorSize i
    prettyPrint0 (T.I64 i) = "i64" ++ printVectorSize i
    prettyPrint0 (T.U8  i) = "u8"  ++ printVectorSize i
    prettyPrint0 (T.U16 i) = "u16" ++ printVectorSize i
    prettyPrint0 (T.U32 i) = "u32" ++ printVectorSize i
    prettyPrint0 (T.U64 i) = "u64" ++ printVectorSize i
    prettyPrint0 (T.F32 i) = "f32" ++ printVectorSize i
    prettyPrint0 (T.F64 i) = "f64" ++ printVectorSize i
    prettyPrint0 (T.Tuple t) = "(" ++ (printList0 ", " t) ++ ")"
    prettyPrint0 (T.Lambda v b) = (prettyPrint0 v) ++ " -> " ++ (prettyPrint0 b)

instance PrettyPrint (E.AExpr a) where
    prettyPrint0 (E.PrimOp _ E.Add [a, b]) = printBinOp a b "+"
    prettyPrint0 (E.PrimOp _ E.Sub [a, b]) = printBinOp a b "-"
    prettyPrint0 (E.PrimOp _ E.Mul [a, b]) = printBinOp a b "*"
    prettyPrint0 (E.PrimOp _ E.Div [a, b]) = printBinOp a b "/"
    prettyPrint0 (E.PrimOp _ (E.Cmp E.Equal)   [a, b]) = printBinOp a b "=="
    prettyPrint0 (E.PrimOp _ (E.Cmp E.NotEqual)[a, b]) = printBinOp a b "!="
    prettyPrint0 (E.PrimOp _ (E.Cmp E.Greater) [a, b]) = printBinOp a b ">"
    prettyPrint0 (E.PrimOp _ (E.Cmp E.Less)    [a, b]) = printBinOp a b "<"
    prettyPrint0 (E.PrimOp _ (E.Cmp E.GreaterEqual) [a, b]) = printBinOp a b ">="
    prettyPrint0 (E.PrimOp _ (E.Cmp E.LessEqual)    [a, b]) = printBinOp a b "<="
    prettyPrint0 (E.PrimOp _ E.RShift [a, b]) = printBinOp a b ">>"
    prettyPrint0 (E.PrimOp _ E.LShift [a, b]) = printBinOp a b "<<"
    prettyPrint0 (E.PrimOp _ E.And [a, b]) = printBinOp a b "&"
    prettyPrint0 (E.PrimOp _ E.Or [a, b]) = printBinOp a b "|"
    prettyPrint0 (E.PrimOp _ E.Xor [a, b]) = printBinOp a b "^"
    prettyPrint0 (E.PrimOp _ E.Select [a, b, c]) = "select " ++ (prettyPrint0 a) ++ " " ++ (prettyPrint0 b) ++ " " ++ (prettyPrint0 c)
    prettyPrint0 (E.PrimOp _ (E.BitCast t) [a]) = "bitcast<" ++ (prettyPrint0 t) ++ "> " ++ (prettyPrint0 a)
    prettyPrint0 (E.PrimOp _ (E.TupleElem i) [a]) = "at<" ++ (show i) ++ "> " ++ (prettyPrint0 a)
    prettyPrint0 (E.Val v) = prettyPrint0 v
    prettyPrint0 _ = error "Cannot print invalid atomic expression"

instance PrettyPrint (E.CExpr a) where
    prettyPrint p@(Printer _) (E.App _ v) = (printList p " " v)
    prettyPrint (Printer ind) (E.If _ c t f) = "if " ++ (prettyPrint0 c) ++ iftrue ++ iffalse
        where
            iftrue = " then\n" ++ sep ++ prettyPrint (Printer $ ind + 1) t ++ "\n"
            iffalse = (indent ind) ++ "else\n" ++ sep ++ prettyPrint (Printer $ ind + 1) f
            sep = indent (ind + 1)
    prettyPrint p (E.Atomic a) = prettyPrint p a

instance PrettyPrint (E.Expr a) where
    prettyPrint (Printer ind) (E.Let _ v b) = "let " ++ bindings ++ " in\n" ++ (indent $ ind + 1) ++ body
        where
            bindingSep = ",\n" ++ indent (ind + 1)
            bindings = (concat (intersperse bindingSep $ map printBinding v))
            body = (prettyPrint (Printer $ ind + 1) b)
            printBinding (w, t, c) = w ++ ": " ++ (prettyPrint0 t) ++ " = " ++ prettyPrint (Printer $ ind + 1) c
    prettyPrint p (E.Complex c) = prettyPrint p c
