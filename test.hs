import System.IO

tokenizeIdentifier :: String -> [String]
tokenizeIdentifier [] = []
tokenizeIdentifier (a : b) = do
  case a of
    x | ('0' <= x && x <= '9') || ('A' <= x && x <= 'Z') || ('a' <= x && x <= 'z') || (x == '_') -> (a : (tokenizeIdentifier b) !! 0) : tail (tokenizeIdentifier b)
    _                                                                                            -> "" : [a:b]

tokenize :: (String, Integer) -> [(String, Integer)]
tokenize ([],_) = []
tokenize (a : b, c) = do
  case a of
    x | ('A' <= x && x <= 'Z') || ('a' <= x && x <= 'z') || (x == '_') -> (tokenizeIdentifier (a : b) !! 0, c) : (tokenize (tokenizeIdentifier (a : b) !! 1, c))
    '\n'                                                               -> tokenize(b, c+1)
    ' '                                                                -> tokenize(b, c)
    '\t'                                                               -> tokenize(b, c)
    '\r'                                                               -> tokenize(b, c)
    '/' -> do
      if b !! 0 == '/'
        then ("//", c) : (tokenize (tail b, c))
      else ("/", c) : (tokenize (tail b, c))
    _                                                                  -> ([a], c) : (tokenize (b, c))

main = do
  filename <- getLine
  withFile filename ReadMode $ \handle -> do
    contents <- hGetContents handle
    let tokens = tokenize (contents, 1)
    print tokens
