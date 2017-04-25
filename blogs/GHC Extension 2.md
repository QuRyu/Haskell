GHC扩展第一篇。

# View Patterns 

### 基本语法

在Haskell中，尽管用户可以使用`case of`关键词来匹配函数调用的结果，但这样做会让代码显得比较丑陋，而我们更希望的是能够使用多个函数而不是`case of`匹配结果。

比如，如下的代码中，我们使用了`case of `关键词来匹配`Map.lookup`函数返回的结果并返回相应的值。

```haskell
lookupMap :: String -> Map.Map String Int -> Int 

lookup k map = case Map.lookup k map of 
					Just n    -> n 
					Nothing -> 0
```

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