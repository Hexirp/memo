# STM

STMモナドはIOモナドとほとんど同じである。おそらくコンパイル時の結果が特別に異なるだけと思われる。たとえば、`retry`は`retry#`というプリミティブで実装されていて、この`retry# :: State# RealWorld -> (# State# RealWorld, a #))`という型をみるに、IOモナド上でも実行できるはずだがそうではない。STMモナドは`IO a -> STM a`という関数を持たない。ゆえに、STMモナドで可能な操作はTVarに関するものに制限されていてロールバックが起きても安全ということだろう。

```haskell
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

newtype STM a = STM (State# RealWorld -> (# State# RealWorld, a #))
```

## 正格に更新する

[IORef](IORef.md)の「正格に更新する」の章でみたように`evaluate`は例外処理においてメリットを持つ。しかし、残念ながらそれはIOモナド専用である．．．即ち、STMモナドの上で更新を行うTVarとは馴染まない。だが、しかし、簡単にSTM上でのevaluateが実装できる！

```haskell
evaluateSTM :: a -> STM a
ebaluateSTM a = STM (\s -> seq# a s)
```
