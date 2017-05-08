本篇是GHC扩展系列的第5篇。今天将会介绍BangPatterns。和上篇的RecordWildCards一样，这个扩展也是非常小巧，却非常实用的一个扩展。



### 什么是BangPatterns

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

对于`negate'`函数来说，Haskell一定会匹配第一个参数的值，但会原封不动的保留第二个参数。



“没有被使用的变量便不求值”，Haskell的惰性求值使得代码更加简洁和优雅。但在一些情况下，因惰性求值采取的将整个式子存储在内存中的策略，导致一些函数的调用会占用过多的内存，比如`foldl`（[原因](http://stackoverflow.com/questions/6872898/haskell-what-is-weak-head-normal-form)）。

假如调用`foldl (+) 0 [1..6]` 的话，会在计算结果得出前在内存中积累大量的数据：

```haskell
foldl (+) 0 [1, 2, 3, 4, 5, 6]
 = foldl (+) (0 + 1) [2, 3, 4, 5, 6]
 = foldl (+) ((0 + 1) + 2) [3, 4, 5, 6]
 = foldl (+) (((0 + 1) + 2) + 3) [4, 5, 6]
 = foldl (+) ((((0 + 1) + 2) + 3) + 4) [5, 6]
 = foldl (+) (((((0 + 1) + 2) + 3) + 4) + 5) [6]
 = foldl (+) ((((((0 + 1) + 2) + 3) + 4) + 5) + 6) []
 = (((((0 + 1) + 2) + 3) + 4) + 5) + 6
 = ((((1 + 2) + 3) + 4) + 5) + 6
 = (((3 + 3) + 4) + 5) + 6
 = ((6 + 4) + 5) + 6
 = (10 + 5) + 6
 = 15 + 6
 = 21
```



但是，如果使用严格求值的话，就可以预先计算出结果，防止内存的浪费。
```haskell
mean :: [Double] -> Double
mean xs = s / fromIntegral l
  where
    (s, l) = foldl' step (0, 0) xs
    step (s, l) a = let s' = s + a
                        l' = l + 1
                    in s' `seq` l' `seq` (s', l')
```
`mean`函数计算一个Double List的平均数。`seq`会对两个参数进行严格求值，并返回第二个参数。

而BangPatterns就是为了简化严格求值的书写方式。使用BangPatterns整个函数可以简化为：
```haskell
mean :: [Double] -> Double
mean xs = s / fromIntegral l
  where
    (s, l) = foldl' step (0, 0) xs
    step (!s, !l) a = (s + a, l + 1)
```
相比于使用`seq`的版本，BangPatterns只用一行变完成了所要做的事情。
