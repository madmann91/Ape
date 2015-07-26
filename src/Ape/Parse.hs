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

boolConstant :: Parser Bool
boolConstant =   do { reserved "true" ; return True }
             <|> do { reserved "false" ; return False }
             <?> "boolean constant"

intConstant :: Num a => Parser a
intConstant = integer >>= (return . fromIntegral) <?> "integer constant"

floatConstant :: (Fractional a) => Parser a
floatConstant = float >>= (return . realToFrac) <?> "floating point constant"

constant :: Parser Expr.Constant
constant =   do { reserved "i8"  ; intConstant >>= (return . Expr.I8)  }
         <|> do { reserved "i16" ; intConstant >>= (return . Expr.I16) }
         <|> do { reserved "i32" ; intConstant >>= (return . Expr.I32) }
         <|> do { reserved "i64" ; intConstant >>= (return . Expr.I64) }
         <|> do { reserved "u8"  ; intConstant >>= (return . Expr.U8)  }
         <|> do { reserved "u16" ; intConstant >>= (return . Expr.U16) }
         <|> do { reserved "u32" ; intConstant >>= (return . Expr.U32) }
         <|> do { reserved "u64" ; intConstant >>= (return . Expr.U64) }
         <|> do { reserved "f32" ; floatConstant >>= (return . Expr.F32) }
         <|> do { reserved "f64" ; floatConstant >>= (return . Expr.F64) }
         <|> do { reserved "i1"  ; boolConstant >>= (return . Expr.I1) }
         <|> ((parens $ constant `sepBy` (symbol ",")) >>= (return . Expr.Tuple))
         <?> "constant"

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
            <|> try lambdaAppExpr
            <|> (atomicExpr >>= (return . Expr.Atomic))
            <?> "complex expression"

lambdaAppExpr :: Parser Expr.CExpr
lambdaAppExpr = do
    x <- value
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
typeName =  try (primType >>= lambdaType p)
         <|> primType
         <?> "type name"

primType =   do { reserved "i8"  ; return Type.I8  }
         <|> do { reserved "i16" ; return Type.I16 }
         <|> do { reserved "i32" ; return Type.I32 }
         <|> do { reserved "i64" ; return Type.I64 }
         <|> do { reserved "u8"  ; return Type.U8  }
         <|> do { reserved "u16" ; return Type.U16 }
         <|> do { reserved "u32" ; return Type.U32 }
         <|> do { reserved "u64" ; return Type.U64 }
         <|> do { reserved "f32" ; return Type.F32 }
         <|> do { reserved "f64" ; return Type.F64 }
         <|> do { reserved "i1"  ; return Type.I1  }
         <|> ((parens $ primType `sepBy` (symbol ",")) >>= (return . Type.Tuple))

lambdaType :: Parser Type.Type
lambdaType v = do
    _ <- symbol "->"
    b <- typeName
    return $ Type.Lambda v b

value :: Parser Expr.Value
value = (constant >>= (return . Expr.Cst))
      <|> (identifier >>= (return . Expr.Var))
      <|> lambdaValue
      <?> "value"

lambdaValue :: Parser Expr.Value
lambdaValue = do
    _ <- reserved "lambda"
    v <- lambdaBinding `sepBy1` (symbol ",")
    _ <- symbol "->"
    e <- expr
    return $ Expr.Lambda v e

lambdaBinding :: Parser (Expr.Variable, Type.Type)
lambdaBinding = do
    v <- identifier
    _ <- symbol ":"
    t <- typeName
    return $ (v, t)

elemExpr :: Parser Expr.AExpr
elemExpr = do { reserved "at" ; i <- angles intConstant ; a <- value ; return $ Expr.PrimOp (Expr.TupleElem i) [a] }
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
        helper a r op = do { reservedOp r; b <- value; return $ Expr.PrimOp op [a, b] }

atomicExpr :: Parser Expr.AExpr
atomicExpr = (try $ parens atomicExpr)
           <|> do { a <- value; binaryExpr a }
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
