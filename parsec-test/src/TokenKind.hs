module TokenKind where

type Token = (TokenKind, String) -- type spitted out by the lexer

makeTokenKind :: (Monad m) => TokenKind -> String -> m Token -- helper function for speed
makeTokenKind tk t = return (tk, t)

data TokenKind
    = Operator
    | OpName -- operator as function name, i.e. `(+)(1, 2)`
    | Arrow -- function type, not parsed as special
    | Syntax -- generic syntax, i.e. type definition, semicolon, assignment, parens, comma
    | Ident
    | Reserved
    | Literal
    | Compiler -- compiler flags, i.e. `#run`
    | Comment
    deriving(Show)