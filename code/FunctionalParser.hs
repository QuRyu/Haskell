module Parser where 

import Prelude hiding (filter)
import Data.Char (isDigit, isSpace, toUpper, ord)

newtype Parser a = Parser {
    runParser :: (String -> [(a, String)])
}

instance Monad Parser where 
    return a = Parser $ \s -> [(a, s)]
    p >>= f = Parser $ \s ->
        concat [runParser (f a) s' | (a, s') <- runParser p s]
       
instance Applicative Parser where 
    pure a = Parser $ \s -> [(a, s)] 
    k <*> m = Parser $ \s -> 
         [(f a, s'') |
           (f, s') <- runParser k s,
           (a, s'') <- runParser m s']
                   
instance Functor Parser where 
   fmap f p = Parser $ \s -> 
        [(f a, s')  | (a, s') <- runParser p s]

applyP :: Parser a -> String -> [(a, String)]
applyP p s = runParser p s

emptyP :: Parser a
emptyP = Parser $ \s -> [] 

appendP :: Parser a-> Parser a-> Parser a 
appendP p q = Parser $ \s -> 
    let xs = runParser p s 
        ys = runParser q s 
    in xs ++ ys

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ \s -> 
        case (runParser (p `appendP` q) s) of 
	    []     -> []
	    (x:xs) -> return x

item :: Parser Char 
item = Parser $ \cs -> 
        case cs of 
            []     -> [] 
            (c:cs) -> [(c, cs)]         

-- since the function tiem is of type "Parser Char"
-- it can only produce char as a result of computation
filterP :: (Char -> Bool) -> Parser Char
filterP f = item >>= \c -> if f c 
          then return c 
          else emptyP 

-- returns ak result if the prefix char matches
char :: Char -> Parser Char 
char c = filterP (\x -> x == c)

-- parses a specific string 
string :: String -> Parser String
string [] = return ""  -- why it will be an empty list if "emptyP" is used? 
string (x:xs) = do c <- char x
                   cs <- string xs 
                   return (c:cs)

many :: Parser a -> Parser [a]
many p = many1 p +++  (return []) 

many1 :: Parser a -> Parser [a]
many1 p = do c <- p 
             cs <- many p 
	     return (c:cs)

sepby :: Parser a -> Parser b -> Parser [a] 
sepby p sep = sepby1 p sep +++ (return [])

sepby1 :: Parser a -> Parser b -> Parser [a] 
sepby1 p sep = do c <- p 
                  cs <- many (sep >> p)
		  return (c:cs)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a 
chainl p q a = (p `chainl1` q) +++ return a 

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a 
p `chainl1` q = do {a <- p; rest a} 
            where rest a = (do  f <- q
	                            b <- p
				                rest (f a b))
			              +++ return a 

space :: Parser String 
space = many (filterP isSpace)  

-- parse a given value, throw away trailing space
token :: Parser a -> Parser a 
token p = do {a <- p; space; return a} 

-- parses a given token, throws away trailing space
symb :: String -> Parser String 
symb s = token (string s)

-- throw away any prefix space, apply parser 
apply :: Parser a -> String -> [(a, String)]
apply p = runParser (do {space; p})

addop :: Parser (Int -> Int -> Int) 
addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}

mulop :: Parser (Int -> Int -> Int)
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}

digit = do {x <- token (filterP isDigit); return (ord x - ord '0')}
factor = digit +++ do {symb "("; n <- expr; symb ")"; return n}
term = factor `chainl1` mulop
expr = term `chainl1` addop
-- expr = ((digit +++ do {symb "("; n <- expr; symb ")"; return n}) `chianl1` mulop) `chainl1` addop


