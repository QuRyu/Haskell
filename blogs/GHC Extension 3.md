### Pattern Synonym 

在Haskell开发的过程中，为了阅读的方便我们可能会使用`type`关键词来替换一些类型的名字，比如用`type Name = String`来表示String类型，这使得代码的意思更佳清晰，同时又不会出现性能上的损失。

而对于类型匹配，GHC同样提供了类似于`type`关键词的扩产，也就是Pattern Synonym。为了节省篇幅，接下来都会有PS关键词来代替Pattern Synonym。



### Pattern Synonym as Constants 

我们将会从Pattern Synonym的最简单的用法开始介绍，也就是用PS来替换代码中会出现的奇怪常量(magic number)。

在Haskell中使用FFI(Foreigh Function Interface)来调用C代码时，如果函数的参数中之一的类型是枚举(enum)，我们向函数传递的其实是int参数。比如，在调用[SDL](http://libsdl.org)库时，其中一个函数是

```c
int SDL_SetRenderDrawBlendMode(SDL_Renderer* renderer,
                               SDL_BlendMode blendMode)
```

第二个参数即为一个枚举形参数。如果想在Haskell中使用这个函数，我们可以通过如下的代码将C中的enum替换为Haskell中的enum:

```haskell
data BlendMode = NoBlending 
			   | AlphaBlending 
			   | AdditiveBlending 
			   | ColourModulatedBlending

toBlendMode :: BlendMode -> CInt
toBlendMode NoBlending = #{const SDL_BLENDMODE_NONE}
toBlendMode AlphaBlending = #{const SDL_BLENDMODE_BLEND}
toBlendMode ...

fromBlendMode :: CInt -> Maybe BlendMode
fromBlendMode 0 = Just NoBlending
fromBlendMode ...
```

todo: find the corresponding explanantion for `#{const}`

这样的处理方式虽然简单，但会减缓运行速度。而使用PS的话，则可以完全避免这样的速度损失。
通过定义
```haskell
pattern NoBlending = #{const SDL_BLENDMODE_NONE} :: CInt
pattern AlphaBlending = #{const SDL_BLENDMODE_BLEND} :: CInt
pattern ...
```
这告诉了GHC，每当程序中出现`NoBlending`、`AlphaBlending`等定义时，实际的类型是CInt，并且值应该和`NoBlending`、`AlphaBlending`等相等。

现在，我们写诸如下列的函数了:
```haskell
setUpBlendMode :: CInt -> IO ()
setUpBlendMode AlphaBlending = do
  putStrLn "Enabling Alpha Blending"
  activateAlphaBlendingForAllTextures
  activateRenderAlphaBlending
```
尽管`setUpBlendMode`的参数是CInt，但通过使用PS，参数的名称简单易懂，并且不会对运行效率产生任何影响。

PS同样也支持newtype的包裹来做到语意的清晰：
```haskell 
newtype BlendMode = MkBlendMode { 
    unBlendMode :: CInt 
}

pattern NoBlending = MkBlendMode #{const SDL_BLENDMODE_NONE}
BlendMode = MkBlendMode { unBlendMode :: CInt }
```
这样一来，我们就可以通过只导出PS，而将`MkBlendMode`隐藏起来，来获得ADT(Abstract Data Type)的便利，又避免了运行效率的下降。


### Bidirectional Patterns
todo: use [Matthew Pickering's Blog](https://mpickering.github.io/posts/2014-11-27-pain-free.html) to explain Bidrectional Patterns
