import System.IO
import Data.Char

data SourcePos = Pos Int Int
data Stream = Stream String SourcePos
type Parser a = String -> Maybe (a, String)

many1 :: Parser a -> Parser [a]
many1 fn str = case fn str of
    Just (l, rem) -> case many1 fn rem of
        Just (ls, rem') -> Just (l:ls, rem')
        Nothing -> Just (l:[], rem)

satisfies :: (Char -> Bool) -> Parser Char
satisfies condition (c:str) = if condition c then Just (c, str) else Nothing

identLexer :: Parser String
identLexer (c:str) = many1 (satisfies isAlphaNum)

intLexer :: Parser Integer
intLexer (c:str)