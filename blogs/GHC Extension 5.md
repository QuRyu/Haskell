本篇是GHC扩展系列的第5篇。今天将会介绍BangPatterns。和上篇的RecordWildCards一样，这个扩展也是非常小巧，却非常实用的一个扩展。



### 什么是BangPatterns

todo: as the title suggests 

BangPatterns能让Haskell的求值方式变为惰性求值(lazy evaluation)变为严格求值(strict evalution)。严格求值是大部分流行语言所采用的求值方式，每一个函数、方法所产生的值都会立即计算出来，而不是将整个式子保存在内存中，等到需要时再进行计算。



### Haskell的惰性求值

那么，Haskell的惰性求值规则是什么呢？

首先，一个值如果不被调用，也就是不对这个值进行匹配的话，Haskell程序便不会求这个值。



```haskell
plain :: Int -> Boolean 
plain n = True 
```

在这个例子中，



todo: Haskell laziness 



todo: list example using BangPatterns and its desugar version

