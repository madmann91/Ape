{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Ape.Parse (irParser, IRParser) where

import qualified Ape.Expr as E
import qualified Ape.Type as T

import qualified Text.Parsec.Token as Tok
import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos

type IRParser = Parser [E.Expr E.Info]

makeInfo :: SourcePos -> SourcePos -> E.Info
makeInfo a b = E.Info undefined $ E.Loc (sourceLine a) (sourceColumn a) (sourceLine b) (sourceColumn b) (sourceName a)

getInfo :: Parser a -> Parser (E.Info, a)
getInfo p = do
    p0 <- getPosition
    r <- p
    p1 <- getPosition
    return (makeInfo p0 p1, r)

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

lambdaValue :: Parser (E.Value E.Info)
lambdaValue = do
    p0 <- getPosition
    _ <- reserved "lambda"
    v <- identifier
    _ <- symbol ":"
    t <- typeName
    e <- expr
    p1 <- getPosition
    return $ E.Lambda (makeInfo p0 p1) v t e

vector :: Parser a -> Parser [a]
vector p =  (angles (sepBy1 p $ symbol ","))
         <|> do { x <- p ; return [x] }

value :: Parser (E.Value E.Info)
value =   do { (i, v) <- getInfo (do { reserved "i8"  ; vector intValue   }) ; return $ E.I8  i v }
      <|> do { (i, v) <- getInfo (do { reserved "i16" ; vector intValue   }) ; return $ E.I16 i v }
      <|> do { (i, v) <- getInfo (do { reserved "i32" ; vector intValue   }) ; return $ E.I32 i v }
      <|> do { (i, v) <- getInfo (do { reserved "i64" ; vector intValue   }) ; return $ E.I64 i v }
      <|> do { (i, v) <- getInfo (do { reserved "u8"  ; vector intValue   }) ; return $ E.U8  i v }
      <|> do { (i, v) <- getInfo (do { reserved "u16" ; vector intValue   }) ; return $ E.U16 i v }
      <|> do { (i, v) <- getInfo (do { reserved "u32" ; vector intValue   }) ; return $ E.U32 i v }
      <|> do { (i, v) <- getInfo (do { reserved "u64" ; vector intValue   }) ; return $ E.U64 i v }
      <|> do { (i, v) <- getInfo (do { reserved "f32" ; vector floatValue }) ; return $ E.F32 i v }
      <|> do { (i, v) <- getInfo (do { reserved "f64" ; vector floatValue }) ; return $ E.F64 i v }
      <|> do { (i, v) <- getInfo (do { reserved "i1"  ; vector boolValue  }) ; return $ E.I1  i v }
      <|> do { (i, v) <- getInfo (parens $ value `sepBy` (symbol ",")) ; return $ E.Tuple i v }
      <|> do { (i, v) <- getInfo identifier ; return $ E.Var i v }
      <|> lambdaValue
      <?> "value"

letExpr :: Parser (E.Expr E.Info)
letExpr = do
    p0 <- getPosition
    reserved "let"
    v <- letBinding `sepBy1` (symbol ",")
    reserved "in"
    b <- expr
    p1 <- getPosition
    return $ E.Let (makeInfo p0 p1) v b

letBinding :: Parser (E.Variable, T.Type, E.CExpr E.Info)
letBinding = do
    v <- identifier
    _ <- symbol ":"
    t <- typeName
    _ <- symbol "="
    e <- complexExpr
    return (v, t, e)

complexExpr :: Parser (E.CExpr E.Info)
complexExpr = ifExpr
            <|> do { v <- value ; (lambdaAppExpr v) <|> (binaryExpr v >>= (return . E.Atomic)) }
            <|> (atomicExpr >>= (return . E.Atomic))
            <?> "complex expression"

lambdaAppExpr :: E.Value E.Info -> Parser (E.CExpr E.Info)
lambdaAppExpr x = do
    p0 <- getPosition
    xs <- many1 value
    p1 <- getPosition
    return $ E.App (makeInfo p0 p1) (x:xs)

ifExpr :: Parser (E.CExpr E.Info)
ifExpr = do
    p0 <- getPosition
    reserved "if"
    c <- value
    reserved "then"
    t <- expr
    reserved "else"
    f <- expr
    p1 <- getPosition
    return $ E.If (makeInfo p0 p1) c t f

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

elemExpr :: Parser (E.AExpr E.Info)
elemExpr = do
    p0 <- getPosition
    reserved "at"
    j <- angles intValue
    v <- value
    p1 <- getPosition
    return $ E.PrimOp (makeInfo p0 p1) (E.TupleElem j) [v]
    <?> "tuple index expression"

bitcastExpr :: Parser (E.AExpr E.Info)
bitcastExpr = do
    p0 <- getPosition
    reserved "bitcast"
    t <- angles typeName
    a <- value
    p1 <- getPosition
    return $ E.PrimOp (makeInfo p0 p1) (E.BitCast t) [a]
    <?> "bitcast expression"

selectExpr :: Parser (E.AExpr E.Info)
selectExpr = do
    p0 <- getPosition
    reserved "select"
    a <- value
    b <- value
    c <- value
    p1 <- getPosition
    return $ E.PrimOp (makeInfo p0 p1) (E.Select) [a, b, c]
    <?> "select expression"

binaryExpr :: E.Value E.Info -> Parser (E.AExpr E.Info)
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
        helper a r op = do
            p0 <- getPosition
            reservedOp r
            b <- value
            p1 <- getPosition
            return $ E.PrimOp (makeInfo p0 p1) op [a, b]

atomicExpr :: Parser (E.AExpr E.Info)
atomicExpr =   do { a <- value ; binaryExpr a }
           <|> (parens atomicExpr)
           <|> elemExpr
           <|> bitcastExpr
           <|> selectExpr
           <?> "atomic expression"

expr :: Parser (E.Expr E.Info)
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
