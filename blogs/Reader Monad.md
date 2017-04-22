本文使用Haskell语言，并需要读者有Monad的基本概念。

### 什么是Reader Monad

在介绍Reader之前，我们先看看Reader的函数签名: 

```haskell
newtype Reader e a = Reader {
    runReader :: e -> a 
}
```

类似于State, Reader内部包含的也是一个函数；但不同之处为State在除了输出结果外还会生成一个新的state，而Reader只是单纯的输出一个结果。



如果不能像State那样实现状态的传递，那么Reader有什么用呢？我们可以先看看下面这个例子。

```haskell
readString :: Reader String [String]
readString = do 
	e <- ask   						-- 获取环境变量
	e' <- local ((++) "hi! ") ask   -- 获取并修改环境变量
	e'' <- ask 						-- 获取环境变量
	return (e : e' : e'' : [])
	
ghci> unline runReader readString "abc"
abc
hi! abc
abc 
```

`ask`的作用类似于State的`get`，而`local`则是在取得环境变量后进行修改。但是不可把`local`和`put`作类比，因为`local`并不能修改下一个`ask`函数所获得的值。无论怎样调用`local`，Reader Monad所传递的环境变量都是不会改变的。这两个函数签名如下：

```haskell
ask :: Reader r r   
local :: (r -> r) -> Reader r a -> Reader r a 
```



Reader的作用大致可以理解为：提供给一个或多个绑定在一起的计算相同的输入值。





### Reader的简单使用

在深入了解Reader和辅助函数的实现之前，先看一个来自官方文档的例子。

```haskell
type Bindings = Map String Int 

isCountCorrect :: Bindings -> Bool 
isCountCorrect = runReader calc_count -- point-free 

calc_count :: Reader Bindings Bool 
calc_count = do 
	bindings <- ask 
	let count = lookupVar "count" bindings
	return (count == (Map.size bindings))


lookupVar :: String -> Bindings -> Int 
lookupVar s = fromJust . Map.lookup s  -- point-free

sampleBindings :: Bindings
sampleBindings = Map.fromList [("abc", 1), ("cbd", 2), ("count", 3)]


ghci> isCountCorrect sampleBindings
True
```

`isCountCorrect`通过使用`calc_count`这个Reader获取结果，所以我们来分析下`calc_count`到底做了什么。

`calc_count`的签名为`Reader Binding Bool`，也就是

```haskell
Reader Binding Bool = Reader {
  runReader :: Binding -> Bool
}
```

在接受类型为`Binding`的变量后，`calc_count`会返回`Bool`变量。这是从外部看`calc_count`的行为，那它具体做了什么呢？

首先，调用`ask`函数获取周围环境，也就是获得`Binding`变量；接着，调用`lookupVar`函数来获取Map中key为`"count"`的值，也就是`3`。最后，对`count`变量和Map的长度进行比较，返回`Bool`值。

而`lookupVar`函数则是根据给定的key来查询值是否存在，如果存在就返回，不存在就抛出异常。



### Reader实现

了解Reader的结构和基本使用方法后，将Reader声明为Monad、Applicative等类的instance以获取更抽象的表达能力。
```haskell
instance Monad (Reader e) where 
    return a = Reader $ const a 
    m >>= f = Reader $ \e -> 
        runReader (f $ runReader m e) e 
 
instance Applicative (Reader e) where 
    pure a = Reader $ const a 
    f <*> m = Reader $ \e -> 
        runReader f e $ runReader m e  

instance Functor (Reader e) where 
    fmap f m = Reader $ \e -> f $ runReader m e 
```

除此之外，Reader还有一系列的辅助函数，比如`ask`、`asks`、`local`等。
```haskell 
ask :: Reader r r 
ask = Reader $ \e -> e 

asks :: (r -> a) -> Reader r a
asks f = do 
    e <- ask 
    return $ f e 	

local :: (r -> r) -> Reader r a -> Reader r a 
local f r = do 
    e <- ask 
    return $ runReader r (f e) 
```
如之前所说，`ask`的作用是从获取环境变量，也就是`e`；`local`是更改一个Reader的环境变量，但不会`local`所在的Reader中其他操作获取到的环境变量。而`asks`的作用可由一个例子来展示:
```haskell
ghci> runReader (asks length) “hi”
2 
```

###Haskell Reader
Haskell中的Reader和State一样，是通过transformer来实现的。ReaderT和Reader的函数签名是:
```haskell
newtype ReaderT r m a = ReaderT {
    runReaderT :: r -> m a 
}

type Reader e a = ReaderT e Identity a 
```

更多的使用例子和ReaderT的实现细节请分别参照
1. https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Reader.html
2. https://hackage.haskell.org/package/transformers-0.5.4.0/docs/Control-Monad-Trans-Reader.html

