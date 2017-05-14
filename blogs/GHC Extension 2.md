GHC扩展第一篇。

comment: View Patterns 只能匹配函数结果。但非函数结果也不用匹配啊hhhhh

# View Patterns 

### 基本语法

在Haskell中，尽管用户可以使用`case of`关键词来匹配函数调用的结果，但这样写出来的代码不够简洁。

比如，如下的代码中，我们使用了`case of `关键词来匹配`Map.lookup`函数返回的结果并返回相应的值。

```haskell
lookupMap :: String -> Map.Map String Int -> Int 

lookup k map = case Map.lookup k map of 
					Just n    -> n 
					Nothing -> 0
```

而View Pattern的作用即在于让我们直接匹配函数调用的结果。上面例子中的代码可以转换成：

```haskell
lookupMap :: String -> Map.map String Int -> Int 

lookupMap s (Map.lookup s -> Just n) = n 

lookupMap _  						 = 0 
```

View Pattern的关键字是`->`。通过使用这个扩展，可以写出多个函数直接匹配结果的代码，相比前一个例子中的代码清爽了很多。

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

GHC will do the exhaustive check for us so we don’t need to worry about failing to check against a special case. c