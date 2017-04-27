### Pattern Synonym 

在Haskell开发的过程中，为了阅读的方便我们可能会使用`type`关键词来替换一些类型的名字，比如用`type Name = String`来表示String类型，这使得代码的意思更佳清晰，同时又不会出现性能上的损失。

而对于类型匹配，GHC同样提供了类似于`type`关键词的扩产，也就是Pattern Synonym。为了节省篇幅，接下来都会有PS关键词来代替Pattern Synonym。



### 一个小例子

我们将会从Pattern Synonym的嘴简单的用法开始介绍，也就是用PS来替换代码中会出现的奇怪常量(magic number)。

在Haskell中使用FFI(Foreigh Function Interface)来调用C代码时，如果函数的参数中之一的类型是枚举(enum)，我们像函数传递的其实是int参数。