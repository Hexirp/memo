==================
IORefについてのメモ
==================

.. highlight:: haskell

IORefについての知識や疑問をここに書く。

正格に更新する
=============

``modifyIORef`` を使うことが出来る。しかし、他にもこのような関数が作れる。

::

  updateIORef :: IORef a -> (a -> a) -> IO ()
  updateIORef ref f = do
    a  <- readIORef ref
    a' <- evaluate (f a)
    writeIORef ref a'

ちなみに、 ``evaluate :: a -> IO a`` は引数を強制的に評価する関数だ。
これはさっき言葉に出した ``modifyIORef'`` でいいんじゃと思われるかもしれないが厳密には違う！

::

  ($!) :: (a -> b) -> a -> b
  f $ a = seq a (f a)

  evaluate :: a -> IO a
  evaluate a = IO (\s -> seq# a s)

  seq :: a -> b -> b
  seq = seq -- プリミティブ

  seq# :: a -> State# s -> (# State# s, a #)
  seq# = seq# -- プリミティブ

これらが大体の前提となる実装である。そして、二つの関数の実装をこのように比較する。

::

  updateIORef :: IORef a -> (a -> a) -> IO ()
  updateIORef ref f = do
    a  <- readIORef ref
    a' <- evaluate (f a)
    writeIORef ref a'

  modfiIORef' :: IORef a -> (a -> a) -> IO ()
  modfiIORef' = do
    a <- readIORef ref
    writeIORef ref $ f a

ここで重要なのは、適用後の結果を正格に評価するために使われる二つの関数、 ``evaluate`` と ``($!)`` の違いである。これは ``evaluate`` のドキュメントに記述されている。

  引数をWHNFへ評価する。
  
  一般的に、 ``evaluate`` は評価が遅延された値に含まれる例外を調べ、（そしておそらくは）その後に対応するため使われる。

  ``evaluate`` が評価するのはWHNFまでである。もっと深く評価したいのであれば、 ``Control.DeepSeq`` の ``force`` 関数が有用である::

    evaluate $ force x

  ``evaluate x`` と ``return $! x`` の間には微妙な違いがあり、それは ``throwIO`` と ``throw`` の違いに似る。遅延された値xが例外を投げるとき、 ``return $! x`` はIOアクションを返すことが出来ず例外が投げられる。一方で、 ``evaluate x`` は常にIOアクションを行う；そのアクションは、評価の際にxが例外を投げるそのときだけ、 *実行* 時に例外を投げる。

  実践上においてこの違いは、 ``(return $! error "foo") >> error "bar"`` は *不正確な* 例外の意味論のせいでコンパイラにより行われる最適化に依存して"foo"か"bar"のどちらも投げうるのに対して、 ``evaluate (error "foo") >> error "bar"`` は"foo"を投げることが保証されている、という形で現れる。

  経験則からいって、よいのは ``evaluate`` を遅延された値の例外の発生を強制するか処理したい時に使うことである。一方、もし効率のために遅延された値の評価をしたくて例外を気にしないのならば、 ``return $! x`` を使用することができる。

  --- ``evaluate`` のドキュメントより拙訳

そのため、例外を考慮するならば、 ``updateIORef`` を使うのが安全ということになる。しかし、その投げられるはずの例外は純粋な関数から発生するものだ。普通、純粋な関数は例外を投げない！（ ``head`` ？あーあーきこえない！）そして、 ``evaluate`` はIOアクションを伴う形で実装されているため、IOアクションのbindなど無駄な処理があり効率が ``($!)`` と比べて遅い。これは、要約すると「 ``evaluate`` は例外の処理、 ``return $!　x`` は効率のために」という感じの最後の一文でも暗示されている。（これらのことは `IORefを正格に更新する関数について : haskell_jp <https://www.reddit.com/r/haskell_jp/comments/8gfspq/iorefを正格に更新する関数について/>`_ で教えていただいた。）
