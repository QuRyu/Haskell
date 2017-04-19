import Data.Monoid ((<>))
import Control.Monad.ST 
import Data.STRef
import Control.Monad
import qualified Data.Map.Lazy as M 
import Data.Maybe (fromJust)
import Data.Functor.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

data Tree a = Leaf a | Branch  (Tree a) (Tree a)

instance Traversable Tree where 
    traverse f (Leaf a) = Leaf <$> f a 
    traverse f (Branch a b) = Branch <$> traverse f a <*> traverse f b

instance Functor Tree where 
    fmap f (Leaf a) = Leaf $ f a 
    fmap f (Branch a b) = Branch (fmap f a) (fmap f b)

instance Foldable Tree where 
    foldMap f (Leaf a) = f a
    foldMap f (Branch a b) = foldMap f a <> foldMap f b 

instance (Show a) => Show (Tree a) where 
    show (Leaf a) = "Leaf " ++ show a 
    show (Branch a b) = "(Branch " ++ show a ++ " " ++ show b ++ ")"

-- instance Foldable 

add :: Int -> Int -> Int 
add = (+) 

square :: Int -> Int 
square x = x * x 

pythagoras :: Int -> Int -> Int 
pythagoras x y = add (square x) (square y)
add' :: Int -> Int -> ((Int -> r) -> r)
add' x y = \k -> k $ add x y

square' :: Int -> ((Int -> r) -> r)
square' x = \k -> k $ square x 

pythagoras' :: Int -> Int -> ((Int -> r) -> r)
pythagoras' x y = \k-> 
    square' x $ \square_x -> 
    square' y $ \square_y -> 
    add' square_x square_y $ k 

thrice :: (a -> a) -> a -> a 
thrice f a = f $ f $ f a 

thrice' :: (a -> (a -> r) -> r) -> a -> ((a -> r) -> r)
thrice' f a = \k -> 
    f a $ \a' -> 
    f a' $ \a'' -> 
    f a'' $ k 

chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
chainCPS s f = \k -> s $ \s' -> f s' $ k 
 
foldST :: (a -> b -> b) -> b -> [a] -> b 
foldST f base xs = runST $ do
    acc <- newSTRef base
    forM_ xs $ \x -> do 
        modifySTRef acc (f x)
    readSTRef acc

class Dual a where 
    dual :: a -> a 

instance Dual Bool where 
    dual = not 

instance Dual Int where 
    dual = negate 


newtype ReaderT r m a = ReaderT {
    runReaderT ::  r -> m a 
}

instance (Monad m) => Monad (ReaderT r m) where 
    return a = ReaderT $ \r -> return a  
    m >>= k = ReaderT $ \r -> 
        runReaderT m r >>= \a -> 
            runReaderT (k a) r 
        
-- ReaderT m (a -> b) 
instance (Applicative m) => Applicative (ReaderT r m) where 
    pure a = ReaderT $ \r -> pure a 
    f <*> m = ReaderT $ \r -> 
        runReaderT f r <*> runReaderT m r 

instance (Functor m) => Functor (ReaderT r m) where 
    fmap f m = ReaderT $ \r -> 
        f <$> runReaderT m r 

instance MonadTrans (ReaderT r) where 
    lift m = ReaderT $ const m 
    

instance (MonadIO m) => MonadIO (ReaderT r m) where 
    liftIO = lift . liftIO 
    
type Reader r = ReaderT r Identity   

runReader :: Reader r a -> r -> a 
runReader r x = runIdentity $ runReaderT r x 

ask :: (Monad m) => ReaderT r m r
ask = ReaderT $ return 

asks :: (Monad m) =>  (r -> a) -> ReaderT r m a
asks f = fmap f ask

local :: (r -> r) -> ReaderT r m a -> ReaderT r m a 
local f m = ReaderT $ \r -> 
    runReaderT m (f r)

type Bindings = M.Map String Int 

isCountCorrect :: Bindings -> Bool 
isCountCorrect b = runReader cal_count b 

cal_count :: Reader Bindings Bool
cal_count = do 
    bindings <- ask 
    let count = lookupVar "count" bindings
    return (count == (M.size bindings))

lookupVar :: String -> Bindings -> Int 
lookupVar s = fromJust . M.lookup s 

sampleBindings :: Bindings
sampleBindings = M.fromList [("aa", 1), ("count", 3), ("3", 12)]


calc_count :: ReaderT String IO Int 
calc_count = do 
    e <- ask 
    liftIO $ putStrLn ("the environment variable " ++ e) 
    e' <- local ((++) " plus ") ask 
    liftIO $ putStrLn ("new environment " ++ e')
    return $ length e + 2 


readString :: Reader String [String]
readString = do 
    e <- ask 
    e' <- local (mappend "hi! ") ask
    e'' <- ask
    return (e : e' : e'' : [])

