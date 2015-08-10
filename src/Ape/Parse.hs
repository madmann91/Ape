{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Ape.Parse (irParser, IRParser) where

import qualified Ape.Expr as Expr
import qualified Ape.Type as Type

import qualified Text.Parsec.Token as T
import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

type IRParser = Parser [Expr.Expr]

irParser :: IRParser
irParser = manyTill expr eof

boolValue :: Parser Bool
boolValue =   do { reserved "true" ; return True }
             <|> do { reserved "false" ; return False }
             <?> "boolean constant"

intValue :: Num a => Parser a
intValue = integer >>= (return . fromIntegral) <?> "integer constant"

floatValue :: (Fractional a) => Parser a
floatValue = float >>= (return . realToFrac) <?> "floating point constant"

lambdaValue :: Parser Expr.Value
lambdaValue = do
    _ <- reserved "lambda"
    v <- identifier
    _ <- symbol ":"
    t <- typeName
    e <- expr
    return $ Expr.Lambda v t e

vector :: Parser a -> Parser [a]
vector p =  (angles (sepBy1 p $ symbol ","))
         <|> do { x <- p ; return [x] }

value :: Parser Expr.Value
value =   do { reserved "i8"  ; (vector intValue) >>= (return . Expr.I8)  }
      <|> do { reserved "i16" ; (vector intValue) >>= (return . Expr.I16) }
      <|> do { reserved "i32" ; (vector intValue) >>= (return . Expr.I32) }
      <|> do { reserved "i64" ; (vector intValue) >>= (return . Expr.I64) }
      <|> do { reserved "u8"  ; (vector intValue) >>= (return . Expr.U8)  }
      <|> do { reserved "u16" ; (vector intValue) >>= (return . Expr.U16) }
      <|> do { reserved "u32" ; (vector intValue) >>= (return . Expr.U32) }
      <|> do { reserved "u64" ; (vector intValue) >>= (return . Expr.U64) }
      <|> do { reserved "f32" ; (vector floatValue) >>= (return . Expr.F32) }
      <|> do { reserved "f64" ; (vector floatValue) >>= (return . Expr.F64) }
      <|> do { reserved "i1"  ; (vector boolValue) >>= (return . Expr.I1) }
      <|> ((parens $ value `sepBy` (symbol ",")) >>= (return . Expr.Tuple))
      <|> (identifier >>= (return . Expr.Var))
      <|> lambdaValue
      <?> "value"

letExpr :: Parser Expr.Expr
letExpr = do
    reserved "let"
    v <- letBinding `sepBy1` (symbol ",")
    reserved "in"
    b <- expr
    return $ Expr.Let v b

letBinding :: Parser (Expr.Variable, Type.Type, Expr.CExpr)
letBinding = do
    v <- identifier
    _ <- symbol ":"
    t <- typeName
    _ <- symbol "="
    e <- complexExpr
    return (v, t, e)

complexExpr :: Parser Expr.CExpr
complexExpr = ifExpr
            <|> do { v <- value ; (lambdaAppExpr v) <|> (binaryExpr v >>= (return . Expr.Atomic)) }
            <|> (atomicExpr >>= (return . Expr.Atomic))
            <?> "complex expression"

lambdaAppExpr :: Expr.Value -> Parser Expr.CExpr
lambdaAppExpr x = do
    xs <- many1 value
    return $ Expr.App (x:xs)

ifExpr :: Parser Expr.CExpr
ifExpr = do
    reserved "if"
    c <- value
    reserved "then"
    t <- expr
    reserved "else"
    f <- expr
    return $ Expr.If c t f

typeName :: Parser Type.Type
typeName =  do { p <- primType ; (lambdaType p) <|> (return p) } <?> "type name"

lambdaType :: Type.Type -> Parser Type.Type
lambdaType v = do
    _ <- symbol "->"
    b <- typeName
    return $ Type.Lambda v b

primType :: Parser Type.Type
primType =   do { reserved "i8"  ; l <- vectorSize ; return (Type.I8  l) }
         <|> do { reserved "i16" ; l <- vectorSize ; return (Type.I16 l) }
         <|> do { reserved "i32" ; l <- vectorSize ; return (Type.I32 l) }
         <|> do { reserved "i64" ; l <- vectorSize ; return (Type.I64 l) }
         <|> do { reserved "u8"  ; l <- vectorSize ; return (Type.U8  l) }
         <|> do { reserved "u16" ; l <- vectorSize ; return (Type.U16 l) }
         <|> do { reserved "u32" ; l <- vectorSize ; return (Type.U32 l) }
         <|> do { reserved "u64" ; l <- vectorSize ; return (Type.U64 l) }
         <|> do { reserved "f32" ; l <- vectorSize ; return (Type.F32 l) }
         <|> do { reserved "f64" ; l <- vectorSize ; return (Type.F64 l) }
         <|> do { reserved "i1"  ; l <- vectorSize ; return (Type.I1  l) }
         <|> ((parens $ typeName `sepBy` (symbol ",")) >>= (return . Type.Tuple))
         where
             vectorSize = (angles intValue) <|> (return 1)

elemExpr :: Parser Expr.AExpr
elemExpr = do { reserved "at" ; i <- angles intValue ; a <- value ; return $ Expr.PrimOp (Expr.TupleElem i) [a] }
         <?> "tuple index expression"

bitcastExpr :: Parser Expr.AExpr
bitcastExpr = do { reserved "bitcast" ; t <- angles typeName ; a <- value ; return $ Expr.PrimOp (Expr.BitCast t) [a] }
            <?> "cast expression"

binaryExpr :: Expr.Value -> Parser Expr.AExpr
binaryExpr l =   helper l "+" Expr.Add
             <|> helper l "-" Expr.Sub
             <|> helper l "*" Expr.Mul
             <|> helper l "/" Expr.Div
             <|> helper l ">>" Expr.RShift
             <|> helper l "<<" Expr.LShift
             <|> helper l ">" (Expr.Cmp Expr.Greater)
             <|> helper l ">=" (Expr.Cmp Expr.GreaterEqual)
             <|> helper l "<" (Expr.Cmp Expr.Less)
             <|> helper l "<=" (Expr.Cmp Expr.LessEqual)
             <|> helper l "==" (Expr.Cmp Expr.LessEqual)
             <|> (return . Expr.Val) l
             <?> "binary expression"
    where
        helper a r op = do { reservedOp r ; b <- value ; return $ Expr.PrimOp op [a, b] }

atomicExpr :: Parser Expr.AExpr
atomicExpr =   do { a <- value ; binaryExpr a }
           <|> (parens atomicExpr)
           <|> elemExpr
           <|> bitcastExpr
           <?> "atomic expression"

expr :: Parser Expr.Expr
expr =   letExpr
     <|> (complexExpr >>= (return . Expr.Complex))
     <?> "expression"

keywords :: [String]
keywords = ["let", "in",
    "if", "then", "else",
    "i1", "i8", "i16", "i32", "i64",
    "u8", "u16", "u32", "u64",
    "f32", "f64",
    "lambda",
    "true", "false",
    "cast", "bitcast", "at"]

langDef = T.LanguageDef ""
    ""
    ";;"
    False
    (letter <|> char '_')
    (letter <|> char '_')
    (oneOf ":!#$%&*+./<=>?@\\^|-~")
    (oneOf ":!#$%&*+./<=>?@\\^|-~")
    keywords
    []
    True

lexer = T.makeTokenParser langDef
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer
symbol = T.symbol lexer
identifier = T.identifier lexer
integer = T.integer lexer
float = T.float lexer
parens = T.parens lexer
angles = T.angles lexer
