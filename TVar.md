# TVar

TVarはIORefの進化版である。IORefはIOモナドの上で操作され、STRefはSTモナドの上で操作されるのに対して、TVarはSTMモナドの上で操作される。STMモナドの操作はIOモナドの上で実行でき、それはアトミックである！さらに、操作が失敗したり条件が満たさなければ巻き戻しできる。

STMモナドはIOモナドと（形式上の実装が）同じである。おそらくコンパイル時の結果が特別に異なるだけと思われる。STMモナドは`IO a -> STM a`という関数を持たない。ゆえに、STMモナドで可能な操作はTVarに関するものに制限されていてロールバックが起きても安全ということだろう。

```haskell
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

newtype STM a = STM (State# RealWorld -> (# State# RealWorld, a #))
```

## evaluate

`evaluate`は例外処理においてメリットを持つ。しかし、残念ながらそれはIOモナド専用である……即ち、STMモナドの上で更新を行うTVarとは馴染まない。だが、しかし、簡単にSTM上でのevaluateが実装できる！

```haskell
evaluateSTM :: a -> STM a
ebaluateSTM a = STM (\s -> seq# a s)
```
