import Prelude hiding (reverse)
import qualified Data.Set as PQ

infixr :> 
data Stream a = a :> Stream a | Nil 

headS :: Stream a -> a 
headS (x :> xs) = x

tailS :: Stream a -> Stream a 
tailS (x :> xs) = xs 

repeatS :: a -> Stream a 
repeatS a = a :> repeatS a 

iterateS :: (a -> a) -> a -> Stream a 
iterateS f a = a :> iterateS f (f a) 

cycleS :: [a] -> Stream a 
cycleS xs = go xs 
    where go []     = go xs
          go (a:as) = a :> go as

fromS :: Num a => a -> Stream a 
fromS n = fromStepS n 1 
    
fromStepS :: Num a => a -> a -> Stream a 
fromStepS x s = x :> fromStepS (x+s) s 

foldrS :: (a -> b -> b) -> Stream a -> b 
foldrS f (x :> xs) = x `f` foldrS f xs 

filterS :: (a -> Bool) -> Stream a -> Stream a 
filterS p (x :> xs) = if p x then x :> filterS p xs else filterS p xs 
                     
takeS :: Int -> Stream a -> [a] 
takeS 0 _         = []
takeS n (x :> xs) = if n > 0 then x : takeS (n-1) xs
                             else [] 

dropS :: Int -> Stream a -> Stream a 
dropS 0 stream    =  stream 
dropS n (x :> xs) = if n > 0 then dropS (n-1) xs 
                             else x :> xs 

splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS 0 stream = ([], stream) 
splitAtS n (s :> ss) = (s:xs, stream) 
    where (xs, stream) = splitAtS (n-1) ss 

zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c 
zipWithS f (a :> as) (b :> bs) = f a b :> zipWithS f as bs 

instance Functor Stream where 
    fmap f (x :> xs) = f x :> fmap f xs 

instance Applicative Stream where 
    pure = repeatS 
    (f :> fs) <*> (x :> xs) = f x :> (fs <*> xs) 

fibS :: Stream Integer 
fibS = 0 :> fib 
    where fib = 1 :> 1 :> zipWithS (+) fib (tailS fib)

primeS :: Stream Integer 
primeS = cycleS primes 

primes :: [Integer] 
primes = 2 : sieve [3, 5..]
    where 
      sieve (x:xs) = x : sieve' xs (insertPrime x xs PQ.empty) 

      sieve' (x:xs) table 
          | nextComposite == x = sieve' xs (adjust x table) 
          | otherwise          = x : sieve' xs (insertPrime x xs table) 
          where 
            (nextComposite, _) = PQ.findMin table 

      adjust x table 
          | n == x    = adjust x (PQ.insert (n', ns) newPQ)
          | otherwise = table 
        where 
          Just ((n, n':ns), newPQ) = PQ.minView table 

      insertPrime p xs = PQ.insert (p*p, map (*p) xs)


isPrime :: Integer -> Bool 
isPrime n = null [ x | x <- [2..bound], n `mod` x == 0]
    where 
      bound :: Integer 
      bound = ceiling . sqrt $ fromIntegral n 

                                   


zipS :: Stream a -> Stream b -> Stream (a, b)
zipS = zipWithS (,)


