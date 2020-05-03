module Parser where

import Data.Bifunctor (Bifunctor(bimap))
import Data.Char
import Data.Functor.Identity
import Data.Functor (($>))

import Text.Parsec
import Text.Parsec.String (Parser)  -- assuming we work on strings
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token

import AST

lexer :: GenTokenParser String () Identity
lexer = makeTokenParser $ emptyDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , identStart = satisfy (\c -> isAsciiUpper c || isAsciiLower c || c == '_')
    , identLetter = satisfy (\c -> isAsciiUpper c || isAsciiLower c || isDigit c || c == '_')
    , nestedComments = True
    , reservedNames = ["true", "false", "if", "while", "mut", "const", "extern"]
    , reservedOpNames = [":", "*", "&", "+", "-", "/"]
    }

-- expressions

expression :: Parser Expr
expression = Identifier <$> identifier lexer
    <|> Literal <$> literal
    <?> "expression"

literal :: Parser Lit
literal = CharLit <$> charLiteral lexer
    <|> StringLit <$> stringLiteral lexer
    <|> reserved lexer "true" $> BoolLit True <|> reserved lexer "false" $> BoolLit False
    <|> either IntLit FloatLit <$> (lexeme lexer sign <*> naturalOrFloat lexer)
    <?> "literal"
  where
    sign = (char '-' $> bimap negate negate) <|> (char '+' $> id) <|> pure id

-- types

typeParser :: Parser Type
typeParser = App <$> identifier lexer <*> genericArgs
    <|> ptrType
    <|> refType
    <|> tupleType
    <|> arrayOrSliceType
    <?> "type"

genericArgs :: Parser [Type]
genericArgs = concat <$> optionMaybe (brackets lexer (typeParser `sepEndBy1` comma lexer))

arrayOrSliceType :: Parser Type
arrayOrSliceType = brackets lexer $ do
    t <- typeParser
    i <- optionMaybe $ semi lexer *> expression
    pure $ case i of
        Just i -> Array t i
        Nothing -> Slice t

argType :: Parser [Type]
argType = typeParser `sepEndBy` comma lexer

tupleType :: Parser Type
tupleType = Tuple <$> parens lexer argType

typeDec :: Parser TypeDec
typeDec = reserved lexer "mut"  $> Mut
    <|> reserved lexer "const"  $> Const
    <|> reserved lexer "extern" $> Extern
    <|> pure None

ptrType :: Parser Type
ptrType = do
    reservedOp lexer "*"
    Ptr <$> typeDec <*> typeParser

refType :: Parser Type
refType = do
    reservedOp lexer "&"
    Ref <$> typeDec <*> typeParser
