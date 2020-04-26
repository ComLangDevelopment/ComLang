module AST where

data Lit
    = Dec Integer
    | Hex Integer
    | Float Double
    | String String
    | Char Char
    | BString String
    | BChar Char
    | Boolean Bool

data Type
    = App Type [Type]
    | Tuple [Type]
    | IntT IntType
    | FloatT FloatType
    | Bool
    | Slice Type
    | Array Type Integer
    | Ptr TypeDec Type
    | Ref TypeDec Type
    | Bottom -- 'never' type
    | Function Type Type

data TypeDec
    = Mut
    | Const
    | Extern
    | None

data IntType
    = I8 | I16 | I32 | I64 | I128 | ISize
    | U8 | U16 | U32 | U64 | U128 | USize

data FloatType
    = F32 | F64