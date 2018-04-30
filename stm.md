# STM

STMモナドはIOモナドとほとんど同じである。おそらくコンパイル時の結果が特別に異なるだけと思われる。たとえば、`retry`は`retry#`というプリミティブで実装されていて、この`retry# :: State# RealWorld -> (# State# RealWorld, a #))`という型をみるに、IOモナド上でも実行できるはずだがそうではない。STMモナドは`IO a -> STM a`という関数を持たない。ゆえに、STMモナドで可能な操作はTVarに関するものに制限されていてロールバックが起きても安全ということだろう。

```haskell
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

newtype STM a = STM (State# RealWorld -> (# State# RealWorld, a #))e
```

## evaluate

STM上でのevaluateが欲しくなった。IORefでの以下のようなコードが書きたい。

```haskell
foo ref f = do
 a  <- readIORef ref
 a' <- evaluate (f a)
 writeIORef ref a'
```

これは`modifyIORef'`でいいんじゃと思われるかもしれないが、厳密にはことなる。

```haskell
evaluate :: a -> IO a
evaluate a = IO (\s -> seq# a s)

($!) :: (a -> b) -> a -> b
f $! a = seq a (f a)

seq :: a -> b -> b
seq = seq

seq# :: a -> State# s -> (# State# s, a #)
seq# = seq#

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO m) = m

(>>=) :: IO a -> (a -> IO b) -> IO b
IO m >>= k = IO (\ s -> case m s of (# new_s, a #) -> unIO (k a) new_s)
```

これらが大体の実装である。そして、`modifyIORef'`の実装は以下の通りである。

```haskell
foo ref f = do
 a  <- readIORef ref
 a' <- evaluate (f a)
 writeIORef ref a'

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' = do
    x <- readIORef ref
    writeIORef ref $! f x
```

展開をしていく。

```haskell
foo ref f
= readIORef ref >>= (\a -> evaluate (f a) >>= (\a' -> writeIORef ref a'))
= IO (\s -> case unIO (readIORef ref) s of (# s', a #) -> unIO (evaluate (f a) >>= (\a' -> writeIORef ref a')) s')
= IO (\s -> case unIO (readIORef ref) s of (# s', a #) -> unIO (
        \s' -> case unIO (evaluate (f a)) s' of (# s'', a' #) -> unIO (writeIORef ref a') s''))
= IO (\s -> case unIO (readIORef ref) s of (# s', a #) -> unIO (
        \s' -> case unIO (IO (\_s -> seq# (f a) _s)) s' of (# s'', a' #) -> unIO (writeIORef ref a') s''))
= IO (\s -> case unIO (readIORef ref) s of (# s', a #) -> unIO (
        \s' -> seq# (f a) s' of (# s'', a' #) -> unIO (writeIORef ref a') s''))

modifyIORef' ref f
= readIORef ref >>= (\x -> writeIORef ref $! f x)
= IO (\s -> case unIO (readIORef ref) s of (# s', x #) -> unIO (writeIORef ref $! f x) s')
```

ここで重要なのは、`evaluate`のドキュメントにあるこのような記述である。

> 引数をWHNFへ評価する。
>
> 一般的に、`evaluate`は評価が遅延された値に含まれる例外を調べ、（たぶん）その後に対応するため使われる。
>
> `evaluate`が評価するのはWHNFまででありそれ以上評価はしない。もっと深く評価したいのであれば、`Control.DeepSeq`の`force`関数が有用である：`haskellevaluate $ force x`
>
> `evaluate x`と`return $! x`の間には微妙な違いがあり、それは`throwIO`と`throw`の違いに似る。遅延された値xが例外を投げるとき、`return $! x`はIOアクションを返すことが出来ず例外が投げられる。一方で、`evaluate x`は常にIOアクションを行う；そのアクションは、評価の際にxが例外を投げるそのときだけ、*実行*時に例外を投げる。
> 実践上でこの違いは、*不正確な例外の*意味論により`(return $! error "foo") >> error "bar"`はコンパイラにより行われる最適化に依存して"foo"か"bar"のどちらかを投げえるのに対して、`evaluate (error "foo") >> error "bar"`は投げることが保証されている、という形で現れる。
> 経験則からいって、よいのは`evaluate`を遅延された値の例外を強制するか処理したい時に使うことである。一方、もし効率のために遅延された値の評価をしたくて例外を気にしないのならば、`return $! x`を使用することが出来ます。
