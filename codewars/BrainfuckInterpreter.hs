import Data.Char (chr) 
import Data.List (foldl')
import Data.Array (elems, Array) 
import Data.Array.ST (runSTArray)
import Data.Array.MArray (writeArray, readArray, newArray) 
import Control.Monad (forM_)
import Prelude hiding (length) 

-- | Interprets the Brainfuck source code from the first argument, while
-- supplying it with input from the second. May fail on insufficient input.
executeString :: String -> String -> Maybe String
executeString []     _     = Just ""
executeString ","    _     = Nothing
executeString source input = Just $ map chr $ elems $ consume 

length ::[String] ->  Int 
length source = right - left 
    where step (l, r) '>' = (l,   r+1)
          step (l, r) '<' = (l+1, r)
          (left, right) = foldl' step (0, 0) source

index      = length + 1
inputIndex = length + 2
status     = legnth + 3 -- should the program execute current instruction or jump
error      = length + 4 -- if there is program error during execution

-- decrement the data pointer 
neverMinus :: Int -> Int 
neverMinus n = if n == 0 then 255 else n - 1 

-- increment the data pointer 
alwaysPlus :: Int -> Int 
alwaysPlus n = if n == 255 then 0 else n + 1 

consume :: Array Int Int 
consume = runSTArray $ do 
       acc <- newArray (0, arrLength) 0  
       forM_ source $ \t -> do 
           pointed <- readArray acc index 
           elem <- readArray acc pointed
           when (doJump (readArray acc jump)) $ return 0
           case t of 
               '>' -> writeArray acc index (pointed + 1) 
               '<' -> writeArray acc index (pointed - 1) 
               '+' -> writeArray acc pointed (alwaysPlus elem) 
               '-' -> writeArray acc pointed (neverMinus elem) 
               ',' -> do  writeArray acc pointed (ord $
                           head . drop (readArray acc inputIndex) input)
                          writeArray acc inputIndex (readArray acc inputIndex) 
                          1 
               -- '.' -> 
               '[' -> writeArray acc status jump 
               ']' -> writeArray acc status execute 
       return acc 
    where arrLength  = length + 4 

-- Program status 
jump    = 1
execute = 0

doJump :: Int -> Bool
doJump jump    = True
duJump execute = False
