这篇是[24 Days of GHC Extension](https://ocharles.org.uk/blog/pages/2014-12-01-24-days-of-ghc-extensions.html)的翻译。



# View Patterns 

### Basic Usage

View patterns allow us to match against the result of function call directly, without the need for “case of”.

For example, the following code:

​    lookupMap :: String -> Map.Map String Int -> Int 

​    lookup k map = case Map.lookup k map of 

​            Just n    -> n 

​            Nothing -> 0

could be converted into:

​    lookupMap :: String -> Map.map String Int -> Int 

​    lookupMap s (Map.lookup s -> Just n) = n 

​    lookupMap _  _                                      = 0 

### View Patterns as Abstraction Tool 

For storing a list of data, we can use List, which is a very primitive data type that performs very inefficiently. For example, look up the last element would take O(n) time. 

Seq, however, supports checking the last element in O(1) time. Seq is implemented as a finger tree, whose data structure is every complicated that we can only use pre-defined functions to manipulate it. Pattern match wouldn’t be easy. But with view pattern we can do this. 

There is ViewR data structure in Seq package that goes as:    

​    data ViewR a = EmptyR | (Seq a) :> a 

​    viewr :: Seq a -> ViewR a 

So we can implement the last operation as:

​    last :: Seq a -> Maybe a 

​    last (viewr -> (Seq xs) :> x) = Just x

​    last _                                  = Nothing 

GHC will do the exhaustive check for us so we don’t need to worry about failing to check against a special case. 