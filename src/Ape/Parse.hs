{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Ape.Parse (irParser, IRParser) where

import qualified Ape.Expr as E
import qualified Ape.Type as T

import qualified Text.Parsec.Token as Tok
import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

type IRParser = Parser [E.Expr]

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

lambdaValue :: Parser E.Value
lambdaValue = do
    _ <- reserved "lambda"
    v <- identifier
    _ <- symbol ":"
    t <- typeName
    e <- expr
    return $ E.Lambda v t e

vector :: Parser a -> Parser [a]
vector p =  (angles (sepBy1 p $ symbol ","))
         <|> do { x <- p ; return [x] }

value :: Parser E.Value
value =   do { reserved "i8"  ; (vector intValue) >>= (return . E.I8)  }
      <|> do { reserved "i16" ; (vector intValue) >>= (return . E.I16) }
      <|> do { reserved "i32" ; (vector intValue) >>= (return . E.I32) }
      <|> do { reserved "i64" ; (vector intValue) >>= (return . E.I64) }
      <|> do { reserved "u8"  ; (vector intValue) >>= (return . E.U8)  }
      <|> do { reserved "u16" ; (vector intValue) >>= (return . E.U16) }
      <|> do { reserved "u32" ; (vector intValue) >>= (return . E.U32) }
      <|> do { reserved "u64" ; (vector intValue) >>= (return . E.U64) }
      <|> do { reserved "f32" ; (vector floatValue) >>= (return . E.F32) }
      <|> do { reserved "f64" ; (vector floatValue) >>= (return . E.F64) }
      <|> do { reserved "i1"  ; (vector boolValue) >>= (return . E.I1) }
      <|> ((parens $ value `sepBy` (symbol ",")) >>= (return . E.Tuple))
      <|> (identifier >>= (return . E.Var))
      <|> lambdaValue
      <?> "value"

letExpr :: Parser E.Expr
letExpr = do
    reserved "let"
    v <- letBinding `sepBy1` (symbol ",")
    reserved "in"
    b <- expr
    return $ E.Let v b

letBinding :: Parser (E.Variable, T.Type, E.CExpr)
letBinding = do
    v <- identifier
    _ <- symbol ":"
    t <- typeName
    _ <- symbol "="
    e <- complexExpr
    return (v, t, e)

complexExpr :: Parser E.CExpr
complexExpr = ifExpr
            <|> do { v <- value ; (lambdaAppExpr v) <|> (binaryExpr v >>= (return . E.Atomic)) }
            <|> (atomicExpr >>= (return . E.Atomic))
            <?> "complex expression"

lambdaAppExpr :: E.Value -> Parser E.CExpr
lambdaAppExpr x = do
    xs <- many1 value
    return $ E.App (x:xs)

ifExpr :: Parser E.CExpr
ifExpr = do
    reserved "if"
    c <- value
    reserved "then"
    t <- expr
    reserved "else"
    f <- expr
    return $ E.If c t f

typeName :: Parser T.Type
typeName =  do { p <- primType ; (lambdaType p) <|> (return p) } <?> "type name"

lambdaType :: T.Type -> Parser T.Type
lambdaType v = do
    _ <- symbol "->"
    b <- typeName
    return $ T.Lambda v b

primType :: Parser T.Type
primType =   do { reserved "i8"  ; l <- vectorSize ; return (T.I8  l) }
         <|> do { reserved "i16" ; l <- vectorSize ; return (T.I16 l) }
         <|> do { reserved "i32" ; l <- vectorSize ; return (T.I32 l) }
         <|> do { reserved "i64" ; l <- vectorSize ; return (T.I64 l) }
         <|> do { reserved "u8"  ; l <- vectorSize ; return (T.U8  l) }
         <|> do { reserved "u16" ; l <- vectorSize ; return (T.U16 l) }
         <|> do { reserved "u32" ; l <- vectorSize ; return (T.U32 l) }
         <|> do { reserved "u64" ; l <- vectorSize ; return (T.U64 l) }
         <|> do { reserved "f32" ; l <- vectorSize ; return (T.F32 l) }
         <|> do { reserved "f64" ; l <- vectorSize ; return (T.F64 l) }
         <|> do { reserved "i1"  ; l <- vectorSize ; return (T.I1  l) }
         <|> ((parens $ typeName `sepBy` (symbol ",")) >>= (return . T.Tuple))
         where
             vectorSize = (angles intValue) <|> (return 1)

elemExpr :: Parser E.AExpr
elemExpr = do { reserved "at" ; i <- angles intValue ; a <- value ; return $ E.PrimOp (E.TupleElem i) [a] }
         <?> "tuple index expression"

bitcastExpr :: Parser E.AExpr
bitcastExpr = do { reserved "bitcast" ; t <- angles typeName ; a <- value ; return $ E.PrimOp (E.BitCast t) [a] }
            <?> "cast expression"

selectExpr :: Parser E.AExpr
selectExpr = do { reserved "select" ; a <- value ; b <- value ; c <- value ; return $ E.PrimOp (E.Select) [a, b, c] }
            <?> "select expression"

binaryExpr :: E.Value -> Parser E.AExpr
binaryExpr l =   helper l "+" E.Add
             <|> helper l "-" E.Sub
             <|> helper l "*" E.Mul
             <|> helper l "/" E.Div
             <|> helper l ">>" E.RShift
             <|> helper l "<<" E.LShift
             <|> helper l "&" E.And
             <|> helper l "|" E.Or
             <|> helper l "^" E.Xor
             <|> helper l ">" (E.Cmp E.Greater)
             <|> helper l ">=" (E.Cmp E.GreaterEqual)
             <|> helper l "<" (E.Cmp E.Less)
             <|> helper l "<=" (E.Cmp E.LessEqual)
             <|> helper l "==" (E.Cmp E.Equal)
             <|> helper l "!=" (E.Cmp E.NotEqual)
             <|> (return . E.Val) l
             <?> "binary expression"
    where
        helper a r op = do { reservedOp r ; b <- value ; return $ E.PrimOp op [a, b] }

atomicExpr :: Parser E.AExpr
atomicExpr =   do { a <- value ; binaryExpr a }
           <|> (parens atomicExpr)
           <|> elemExpr
           <|> bitcastExpr
           <|> selectExpr
           <?> "atomic expression"

expr :: Parser E.Expr
expr =   letExpr
     <|> (complexExpr >>= (return . E.Complex))
     <?> "expression"

keywords :: [String]
keywords = ["let", "in",
    "if", "then", "else",
    "i1", "i8", "i16", "i32", "i64",
    "u8", "u16", "u32", "u64",
    "f32", "f64",
    "lambda",
    "true", "false",
    "select",
    "cast", "bitcast", "at"]

langDef = Tok.LanguageDef ""
    ""
    ";"
    False
    (letter <|> char '_')
    (alphaNum <|> char '_')
    (oneOf ":!#$%&*+./<=>?@\\^|-~")
    (oneOf ":!#$%&*+./<=>?@\\^|-~")
    keywords
    []
    True

lexer = Tok.makeTokenParser langDef
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
symbol = Tok.symbol lexer
identifier = Tok.identifier lexer
integer = Tok.integer lexer
float = Tok.float lexer
parens = Tok.parens lexer
angles = Tok.angles lexer
