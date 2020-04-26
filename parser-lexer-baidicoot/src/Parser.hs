module Parser where

import Text.Parsec.String (Parser) -- assuming we work on strings
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Token

import Data.Functor.Identity
import Data.Functor (($>))
import Control.Applicative hiding(Const)

import AST

lexer :: GenTokenParser String () Identity
lexer = makeTokenParser $ emptyDef {
    reservedOpNames = [";", ":"],
    commentStart = "/*", commentEnd = "*/",
    commentLine = "//",
    nestedComments = True,
    reservedNames = []
}

arrayType :: Parser Type
arrayType = do
    char '['
    t <- typeParser
    char ';'
    i <- integer lexer
    char ']'
    return (Array t i)

sliceType :: Parser Type
sliceType = do
    char '['
    t <- typeParser
    char ']'
    return (Slice t)

argType :: Parser [Type]
argType = do
    t <- typeParser
    char ','
    ts <- argType
    return (t:ts)

tupleType :: Parser Type
tupleType = Tuple <$> parens lexer argType

typeDec :: Parser TypeDec
typeDec =
        symbol lexer "mut"      $> Mut
    <|> symbol lexer "const"    $> Const
    <|> symbol lexer "extern"   $> Extern
    <|> return None

ptrType :: Parser Type
ptrType = do
    char '*'
    d <- typeDec
    t <- typeParser
    return (Ptr d t)

refType :: Parser Type
refType = do
    char '&'
    d <- typeDec
    t <- typeParser
    return (Ptr d t)

builtin :: Parser Type
builtin =
        symbol lexer "bool"     $> Bool
    <|> symbol lexer "!"        $> Bottom
    <|> symbol lexer "u8"       $> IntT U8
    <|> symbol lexer "u16"      $> IntT U16
    <|> symbol lexer "u32"      $> IntT U32
    <|> symbol lexer "u64"      $> IntT U64
    <|> symbol lexer "u128"     $> IntT U128
    <|> symbol lexer "usize"    $> IntT USize
    <|> symbol lexer "i8"       $> IntT I8
    <|> symbol lexer "i16"      $> IntT I16
    <|> symbol lexer "i32"      $> IntT I32
    <|> symbol lexer "i64"      $> IntT I64
    <|> symbol lexer "i128"     $> IntT I128
    <|> symbol lexer "isize"    $> IntT ISize

typeParser :: Parser Type
typeParser =
        builtin
    <|> ptrType
    <|> refType
    <|> tupleType
    <|> sliceType
    <|> arrayType