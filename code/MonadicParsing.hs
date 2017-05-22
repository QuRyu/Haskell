import Control.Monad (MonadPlus, mzero, mplus)
import Control.Applicative (Alternative, empty, (<|>))
import Data.Char (isSpace)

-- a non-empty list denotes parse success 
newtype Parser a = Parser {
    parser :: String -> [(a, String)]
}

instance Functor Parser where 
    fmap f p = Parser $ \cs -> 
        [(f a, cs') | (a, cs') <- parser p cs]
            
instance Applicative Parser where 
    pure a = return a 
    f <*> m = Parser $ \cs -> 
        [(f a, cs'') | (f, cs')  <- parser f cs,
                       (a, cs'') <- parser m cs']

instance Monad Parser where 
    return a = Parser $ \cs -> [(a, cs)] -- the parser "return a" succeeds 
                                         -- without consuming any of argument
                                         -- string 
    m >>= k  = Parser $ \cs -> 
        concat [parser (k a) cs' | (a, cs') <- parser m cs]

instance Alternative Parser where 
    empty = mzero 
    (<|>)   = mplus 

instance MonadPlus Parser where 
   mzero = Parser $ \cs -> [] 
   p `mplus` q = Parser $ \cs ->
       parser p cs ++ parser q cs 

-- deterministic selector that only picks the 
-- first one from many possible results 
(+++) :: Parser a -> Parser a -> Parser a 
p +++ q = Parser $ \cs -> 
    case (parser p cs ++ parser q cs) of 
        []     -> []
        (x:xs) -> [x]
                                 

item :: Parser Char 
item = Parser $ \cs -> 
    case cs of 
        []     -> []
        (x:xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char 
sat p = do a <- item 
           if p a then return a 
                  else mzero 

char :: Char -> Parser Char 
char c = sat (== c)

string :: String -> Parser String 
string "" = return ""
string s@(x:xs) = do char x 
                     string xs 
                     return s

-- apply a parser zero or more times 
many :: Parser a -> Parser [a] 
many p = many1 p +++ return [] 

-- apply a parser one or more times 
many1 :: Parser a -> Parser [a]
many1 p = do a <- p 
             as <- many p 
             return (a:as)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a 
chainl p q a = chainl1 p q +++ return a 

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a 
chainl1 p q = do a <- p 
                 rest a 
    where rest a = (do f <- q 
                       b <- p 
                       rest $ f a b)
                   +++ return a 


space :: Parser String 
space = many (sat isSpace)

-- throw away any trailing space 
token :: Parser a -> Parser a 
token p = do a <- p 
             space 
             return a 

-- parse a symbolic token 
symb :: String -> Parser String 
symb cs = token (string cs)

-- apply a parser and throw away any leading space 
apply :: Parser a -> String -> [(a, String)]
apply p = parser $ do {space; p} 
