module Lexer where

import Text.Parsec.String (Parser) -- assuming we work on strings
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Token

import Data.Functor.Identity
import Control.Applicative

import TokenKind

lexer :: GenTokenParser String () Identity
lexer = makeTokenParser $ emptyDef {
    reservedOpNames = [";", ":"], -- i'm guessing
    commentStart = "/*", commentEnd = "*/",
    commentLine = "//",
    nestedComments = True, -- my preference
    reservedNames = [] -- flesh out, do we even need reserved names?
}

wrapParens :: Parser a -> Parser a
wrapParens parser = do
    char '('
    d <- parser
    char ')'
    return d

int :: Parser Token -- GenTokenParser automatically converts to `Integer`
int = do
    signs <- many (char '-')
    dat <- many1 digit
    return (Literal, signs ++ dat)

ident :: Parser Token
ident = identifier lexer >>= makeTokenKind Ident

op :: Parser Token
op = operator lexer >>= makeTokenKind Operator

parensOp :: Parser Token
parensOp = wrapParens (operator lexer) >>= makeTokenKind OpName

str :: Parser Token
str = stringLiteral lexer >>= makeTokenKind Literal

flt :: Parser Token
flt = do
    (_, i) <- int
    char '.'
    f <- many1 digit
    return (Literal, i ++ ('.':f))

syntax :: Parser Token
syntax = oneOf ":;(){}" >>= (\c -> return [c]) >>= makeTokenKind Syntax

arr :: Parser Token
arr = string "->" >>= makeTokenKind Arrow

expr :: Parser Token
expr = parensOp
    <|> op
    -- <|> reserve
    <|> ident
    <|> int
    <|> str
    <|> flt
    <|> syntax
    <|> arr