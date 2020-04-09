module Lexer where

import Text.Parsec.Text.Lazy (Parser) -- assuming we work on strings
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.GenTokenParser Text () Identity
lexer = makeTokenParser $ emptyDef {
    reservedOpNames = [";", ":"], -- i'm guessing
    commentStart = "/*", commentEnd = "*/",
    commentLine = "//",
    nestedComments = True, -- my preference
    reservedNames = [] -- flesh out, do we even need reserved names?
}