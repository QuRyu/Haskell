本篇是GHC扩展系列的第5篇。今天将会介绍BangPatterns。和上篇的RecordWildCards一样，这个扩展也是非常小巧，却非常实用的一个扩展。



### 什么是BangPatterns

todo: as the title suggests 

BangPatterns能让Haskell的求值方式变为惰性求值(lazy evaluation)变为严格求值(strict evalution)。严格求值是大部分流行语言所采用的求值方式，每一个函数、方法所产生的值都会立即计算出来，而不是将整个式子保存在内存中，等到需要时再进行计算。



### Haskell的惰性求值

那么，Haskell的惰性求值规则是什么呢？

首先，一个值如果不被调用，也就是不对这个值进行匹配的话，Haskell程序便不会求这个值。



```haskell
plain :: Int -> Bool 
plain n = True 
```

尽管参数`n :: Int`被传给了`plain`，但因为`plain`没有使用这个值，Haskell便不会求出n的值。

```haskell
ghci> plain undefined
True 
```

也就是说，即使传入了`undefined`，也是不会报错的，因为参数没有被匹配。



```haskell
negate' :: Bool -> a -> Bool
negate' True  a = False 
negate' False a = True
```

对于`negate'`函数来说，Haskell一定会求出第一个参数的值，但会原封不动的保留第二个参数。



### BangPatterns的使用场景

todo: foldl example using BangPatterns (why strict) and its desugar version

