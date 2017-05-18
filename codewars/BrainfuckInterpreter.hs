import Data.Char (chr) 
import Data.List (foldl')
import Data.Array (elems, Array) 
import Data.Array.ST (runSTArray)
import Data.Array.MArray (writeArray, readArray, newArray) 
import Control.Monad (forM_)

import Control.Monad.Trans.State (StateT, get, put, state)

-- | Interprets the Brainfuck source code from the first argument, while
-- supplying it with input from the second. May fail on insufficient input.
executeString' :: String -> String -> Maybe String
executeString' []     _     = Just ""
executeString' ","    _     = Nothing
-- executeString' source input = Just $ map chr $ elems $ consume 

length' ::String ->  Int 
length' source = right - left 
    where step (l, r) '>' = (l,   r+1)
          step (l, r) '<' = (l+1, r)
          (left, right) = foldl' step (0, 0) source

-- decrement the data pointer 
neverMinus :: Int -> Int 
neverMinus n = if n == 0 then 255 else n - 1 

-- increment the data pointer 
alwaysPlus :: Int -> Int 
alwaysPlus n = if n == 255 then 0 else n + 1 

consume :: String -> String -> (Array Int Int, Array Int Char) 
consume source input = runSTArray $ do 
       pointer <- newArray (0, arrlength') 0  
       forM_ source $ \t -> do 
           pointed <- readArray pointer point 
           elem <- readArray pointer pointed
           isWrong <- readArray pointer error 
           status' <- readArray pointer status 
           when (1 == isWrong) $ return 0 
           when (doJump status') $ return 0
           case t of 
               '>' -> writeArray pointer point (pointed + 1) 
               '<' -> writeArray pointer point (pointed - 1) 
               '+' -> writeArray pointer pointed (alwaysPlus elem) 
               '-' -> writeArray pointer pointed (neverMinus elem) 
               ',' -> do  index <- readArray pointer inputIndex 
                          writeArray pointer pointed (ord $
                                head . drop index input)
                          writeArray pointer inputIndex (index+1)
                          1 
               '[' -> writeArray pointer status jump 
               ']' -> writeArray pointer status execute 
       return pointer
    where arrlength'  = length'' + 4 
          length'' = length' source 
          strlength' = 1 +  foldl' (\count s -> case s of 
                                                 '.' -> count + 1) 0 source 
          point      = length'' + 1
          inputIndex = length'' + 2
          status     = length'' + 3 -- should the program execute current instruction or jump
          error      = length'' + 4 -- if there is program error during execution

-- Program status 
jump    = 1
execute = 0

doJump :: Int -> Bool
doJump jump    = True
duJump execute = False


type ParserT = StateT 
-- +, -, [, ], ',', '.', '>', '<' 
-- StateT to pass array and output string 
executeString :: String -> String -> Maybe String 
executeString source input = undefined 

        
     
