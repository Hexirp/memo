# IORef

IORefについてのメモ。

## 正格に更新する

以下のような関数は有用であろうか？`evaluate :: a -> IO a`は引数を強制的に評価する関数だ。

```haskell
updateIORef ref f = do
 a  <- readIORef ref
 a' <- evaluate (f a)
 writeIORef ref a'
```

この関数の意味は、refの参照先へ正格にfを適用することである。`modifyIORef'`でいいんじゃと思われるかもしれないが、厳密には違う！

```haskell
($!) :: (a -> b) -> a -> b
f $! a = seq a (f a)

evaluate :: a -> IO a
evaluate a = IO (\s -> seq# a s)

seq :: a -> b -> b
seq = seq -- プリミティブ

seq# :: a -> State# s -> (# State# s, a #)
seq# = seq# -- プリミティブ
```

これらが大体の中身である。そして、`modifyIORef'`の実装は以下の通りである。

```haskell
updateIORef ref f = do
 a  <- readIORef ref
 a' <- evaluate (f a)
 writeIORef ref a'

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' = do
    x <- readIORef ref
    writeIORef ref $! f x
```

ここで重要なのは、適用後の結果を正確に評価するために使われる二つの関数、`evaluate`と`($!)`の違いである。これを理解するに`evaluate`のドキュメントにあるこのような記述である。

> （拙訳）
>
> 引数をWHNFへ評価する。
>
> 一般的に、`evaluate`は評価が遅延された値に含まれる例外を調べ、（おそらく）その後に対応するため使われる。
>
> `evaluate`が評価するのはWHNFまでである。もっと深く評価したいのであれば、`Control.DeepSeq`の`force`関数が有用である：`evaluate $ force x`
>
> `evaluate x`と`return $! x`の間には微妙な違いがあり、それは`throwIO`と`throw`の違いに似る。遅延された値xが例外を投げるとき、`return $! x`はIOアクションを返すことが出来ず例外が投げられる。一方で、`evaluate x`は常にIOアクションを行う；そのアクションは、評価の際にxが例外を投げるそのときだけ、*実行*時に例外を投げる。
> 実践上においてこの違いは、`(return $! error "foo") >> error "bar"`は*不正確な*例外の意味論のせいでコンパイラにより行われる最適化に依存して"foo"か"bar"のどちらも投げうるのに対して、`evaluate (error "foo") >> error "bar"`は"foo"を投げることが保証されている、という形で現れる。
> 経験則からいって、よいのは`evaluate`を遅延された値の例外の発生を強制するか処理したい時に使うことである。一方、もし効率のために遅延された値の評価をしたくて例外を気にしないのならば、`return $! x`を使用することが出来る。

ゆえに、例外を考えるのならば`evaluate`を使うのが安全ということになる。`($!)`より`evaluate`の方が良い。だが、そんなうまい話があるのか？`updateIORef`をいつでも使うべきなのか？`evaluate`のデメリットは？
