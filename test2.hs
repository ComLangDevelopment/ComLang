import System.IO
import Data.Char

data SourcePos = Pos Int Int

nextPos :: SourcePos -> SourcePos
nextPos (Pos a b) = Pos a (b+1)

data Stream = Stream String SourcePos

getPos :: Stream -> SourcePos
getPos (Stream _ p) = p

type Parser a = Stream -> Maybe (a, Stream)

many1 :: Parser a -> Parser [a]
many1 fn str = case fn str of
    Just (l, rem) -> case many1 fn rem of
        Just (ls, rem') -> Just (l:ls, rem')
        Nothing -> Just (l:[], rem)

recordPos :: Parser a -> Parser (SourcePos, a)
recordPos fn str = let pos = getPos str in fmap (\(a, rem) -> ((pos, a), rem)) (fn str)

satisfies :: (Char -> Bool) -> Parser Char
satisfies condition (Stream (c:str) pos) = if condition c then Just (c, (Stream str (nextPos pos))) else Nothing

identLexer :: Parser String
identLexer = many1 (satisfies isAlphaNum)