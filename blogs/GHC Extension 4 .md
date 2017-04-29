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



todo: use RecordWildCards to create data type 

todo: advanced usage of RecordWildCards (manage module imports)


