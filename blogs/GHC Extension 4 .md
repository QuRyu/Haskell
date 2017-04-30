今天是GHC扩展系列文章的第四篇，RecordWildCards. 

RecordWildCards不是一个功能非常厉害的扩展，相反，它只是一个能帮助程序代码更佳清晰的小技巧。但正是这些小技巧的使用，能让整体代码非常的清爽。

### 什么是Records

在了解今天的扩展前，我们先明确Records的含义。

Records即为其表面意思，“记录”，也就是我们用来模拟现实世界的数据类型，所以可以说是“对现实世界的记录”，比如

```haskell
data Person = Person {
  name :: String, 
  age  :: Int 
}
```

简单的Person类型便是record。



### RecordWildCards

在使用这个扩展前，需要在文件顶部申明

```haskell
{-# LANGUAGE RecordWildCards #-}
```

这等同是告诉GHC，“我要使用RecordWildCards扩展了，如果在程序中遇到了这个扩展的使用，请记得进行对应的处理“。

在介绍使用方法前，我们先看看在哪些地方可能使用到这个扩展吧。



设想我们现在定义了一个数据类型Worker 

```haskell
data Worker = Worker {
  workerName  :: String
, workerAge   :: Int
, workerTitle :: String
}
```

而该程序需要与其他的程序进行网络上的数据交换，而现在网络上较常见的是使用JSON格式(JavaScript Object Notation)来进行操作，于是，我们需要将Worker定义为类型系统中的`ToJSON `和`FromJSON`类的实例。

```haskell
instance ToJSON Worker where
  toJSON w = object [ "name"  .= workerName w
                    , "age"   .= workerAge w  
                    , "title" .= workerTitle w 
                    ]
```

为了获取Worker内部的变量，我们每次都要调用不同的accessor function获取数据。而RecordWildCards便是用来简化“获取内部数据”步骤的。其使用方法见如下示例

```haskell
instance ToJSON Worker where 
  toJSON Worker{..} = object [ "name"  .= workerName
  						     , "age"   .= workerAge 
  						     , "title" .= workerTitle 
  						     ]
```

正如例子中所示，只需要将`w`替换为`Worker{..}`，也就是`数据类型+{..}`，就可以直接调用其accessor function，而不需要在其后加上变量，来获得Worker内部的数据了。

上述功能便是RecordWildCards所能提供的全部了。尽管这个功能可以说是微不足道的，但它还能发挥一些意想不到的作用。



### 使用RecordWildCards管理imports 

在每一个文件内，都要在开头写上各种的imports来引入其他的模块(module)。而如果遇到了有包含相同函数名的两个模块，则常常会使用`import qualified … as …`来避免名称的冲突。比如同时使用`Data.ByteString`和`Data.ByteString.Lazy`，通常会将将其引用分别限制为`S`和`L`。

但这样带来的不便则是每调用一个名称有冲突的函数，都需要在前面写上`S`或`L`。但RecordWildCards为我们提供了不需要使用限制名称的方法。



假设我现在想分别使用`Data.ByteString` 和`Data.ByteString.Lazy`中的方法。
```haskell
import qualified Data.ByteString as S 
import qualified Data.ByteString.Lazy as L 
```

但和以往直接使用`S.<function>`不同的是，我们先定义一个数据类型，其中包含了每一种我们可能会用到的方法。
```haskell
data StringModule s = String
  { map :: (Word8 -> Word8) -> s -> s
  , concatMap :: (Word8 -> s) -> s -> s
  , filter :: (Word8 -> Bool) -> s -> s
  , length :: s -> Int
  , singleton :: Word8 -> s
  , null :: s -> Bool
  , pack :: [Word8] -> s
  , unpack :: s -> [Word8]
  , empty :: s
  , readFile :: FilePath -> IO s
  , writeFile :: FilePath -> s -> IO ()
  , break :: (Word8 -> Bool) -> s -> (s, s)
  , span :: (Word8 -> Bool) -> s -> (s, s)
  , dropWhile :: (Word8 -> Bool) -> s -> s
  , takeWhile :: (Word8 -> Bool) -> s -> s
  , any :: (Word8 -> Bool) -> s -> Bool
  , all :: (Word8 -> Bool) -> s -> Bool
  , splitAt :: Int -> s -> (s, s)
  }
```

然后，在同一个文件中，我们创建两个类型为StringModule的数据。
```haskell
lazyByteString :: StringModule 
lazyByteString = String 
  { map       = L.map
  , concatMap = L.concatMap
  , ...
  }
    
strictByteString = StringModule 
  { map       = S.map 
  , concatMap = S.concatMap
  , ...
  }

```

那这样有什么好处呢？
在定义完后，我们想使用其中一个模块下的方法时，可以借助RecordWildCards消除限定名称的使用
```haskell
zot :: (a, b) 
zot = (a, b) where 
    a = pack [1, 2, 3] where String{..} = lazyByteString
    b = pack [1, 2, 3] where String{..} = strictByteString 
```
在这里，尽管我们调用了两个模块的相同名称方法，但并不会引起冲突，并且省去了限定词`L`、`S`的麻烦。



###总结
RecordWildCards是GHC扩展中的一个小巧、方便的拓展，能达到简化代码的目的。
