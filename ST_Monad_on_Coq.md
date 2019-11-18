# ST Monad on Coq

ST s モナドを Coq で実装します。発端は[このツイート](https://twitter.com/mod_poppo/status/1171772482891612160)を参照してください。

## s を特殊化する

ST s モナドの中身はこんな感じです（実際は Unboxed 型を使っている）。

```haskell
data ST s a = ST { unST :: s -> (a, s) }
```

ここで `runST :: (forall s. ST s a) -> a` は `s` を特殊化すればいけるのではないかと目途を立てることができます。

さて、この `s` は何になるのでしょうか。簡単にいえば、型の配列と値の配列だとなるでしょう。それは `STRef s a` というものの内部を `Int` としたとき、自然にそうなります。

[ここ](https://github.com/Hexirp/coq-gist/blob/2d02f8da731425b493041045a31712965790a180/ST.v)で実装をしてみました。しかし、必要な操作、つまり `getSTRef :: STRef s a -> ST s a` などを実装しようとしたとき、行き詰ってしまいました。

これは、一度辞書に格納した型を再び取り出そうとしたときに、型が元々と等しいものなのか保証することができないからです。例えば `Typeable` など、といった安全ではない手段を使えば、できるのでしょうが、さっきも言った通り安全に実装する必要があります。

## Operational Monad and Indexed Monad

どうすればいいのでしょうか。問題の核は `ST s a` という型の値が自由に作れる、つまり辞書をクリアするような操作も作れてしまうことにあります。

これを Operational Monad で制限しましょう。

```haskell
data ST (s :: Type) (a :: Type) :: Type where
  NewSTRef :: a -> ST s (STRef s a)
  GetSTRef :: STRef s a -> ST s a
  SetSTRef :: STRef s a -> a -> ST s ()
```

さらに、辞書をこのように組み立てます。

```haskell
data TypeDict :: Type where
  MkTypeDict :: (Nat -> Maybe Type) -> TypeDict

data Dict (s :: Type) (tl :: Nat -> Maybe Type) :: Type where
  MkDict :: (forall (i :: Nat) (t :: Type), unTypeDict tl i :~: Just t -> t) -> Dict s tl
```

ここで `s` は単に `Dict` と `STRef` が漏れ出すのを防ぐための幽霊型にすぎません（後で気が付いたのですが、この実装なら `s` のカインドも自由にできますね）。

```haskell
data STRef (s :: Type) (a :: Type) :: Type where
  MkSTRef :: Nat -> STRef s a
```

次に、実行による辞書の更新を表す関数を作ります。

```haskell
runTypeST :: Operational (ST s) a -> TypeDict -> TypeDict
```

最後に、実行する関数を作ります。

```haskell
runST :: (x :: Operational (ST s) a) -> forall (tl :: TypeDict). Dict s tl -> (a, Dict s (runTypeST x tl))
```

さらに、辞書のデフォルト値を与えれば終わりです。

```haskell
runST :: (forall s. Operational (ST s) a) -> a
```

では、実装してみましょう。
