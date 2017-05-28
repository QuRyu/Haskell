import Prelude hiding (reverse)

infixr :> 
data Stream a = a :> Stream a | Nil 

headS :: Stream a -> a 
headS (x :> xs) = x

tailS :: Stream a -> Stream a 
tailS (x :> xs) = xs 

repeatS :: a -> Stream a 
repeatS a = a :> repeatS a 

iterateS :: (a -> a) -> a -> Stream a 
iterateS f a = repeatS $ f a 

cycleS :: [a] -> Stream a 
cycleS xs = go xs 
    where go []     = go xs
          go (a:as) = a :> go as

fromS :: Num a => a -> Stream a 
fromS n = n :> fromS (n+1)  
    
fromStepS :: Num a => a -> a -> Stream a 
fromStepS x s = undefined 

foldrS :: (a -> b -> b) -> Stream a -> b 
foldrS f (x :> xs) = x `f` foldrS f xs 

filterS :: (a -> Bool) -> Stream a -> Stream a 
filterS p (x :> xs) = if p x then x :> filterS p xs else filterS p xs 
                     
takeS :: Int -> Stream a -> [a] 
takeS 0 _         = []
takeS n (x :> xs) = x : takeS (n-1) xs

dropS :: Int -> Stream a -> Stream a 
dropS 0 stream    =  stream 
dropS n (x :> xs) = dropS (n-1) xs 

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

scanlS :: (b -> a -> b) -> b -> Stream a -> Stream b 
scanlS              = go 
    where 
      go :: (b -> a -> b) -> b -> Stream a -> Stream b 
      go f acc (x :> xs) = acc :> go f (f acc x) xs 

fibS :: Stream Integer 
fibS = scanlS (+) 0 $ 1 :> 1 :> fibS 

primeS :: Stream Integer 
primeS = filterS isPrime (fromS 2)
    where 
      isPrime :: Integer -> Bool 
      isPrime n = foldr (\num cond -> 
                                case cond of 
                                    False -> False 
                                    True  -> n `mod` num /= 0)
                                   True [2..n] 

sample = 1 :> (2 :> 3 :> Nil)
stream1 = repeatS 1 


