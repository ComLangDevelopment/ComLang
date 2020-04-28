module AST where

data Expr
    = Literal Lit
    deriving Show

data Lit
    = IntLit Integer
    | FloatLit Double
    | StringLit String
    | CharLit Char
    | BStringLit String
    | BCharit Char
    | BoolLit Bool
    deriving Show

data Type
    = App String [Type]
    | Tuple [Type]
    | Slice Type
    | Array Type Expr
    | Ptr TypeDec Type
    | Ref TypeDec Type
    | Bottom  -- 'never' type
    | Function [Type] Type
    deriving Show

data TypeDec
    = Mut
    | Const
    | Extern
    | None
    deriving Show
