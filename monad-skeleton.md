# monad-skeleton

monad-skeletonはSkeletonモナド、またはFreerモナドを提供する小さくまとまったライブラリ。

## Freeモナド

```haskell
data Free f a = Pure a | FreeC (f (Free f a))
```

Freeモナド。「こんな関数があるモナドが欲しい！」と思うことがある。例えば、putStrLnに似た関数`putStrLnM :: String -> m ()`を持つモナドmとか。
そういう関数を適切な形で型に変換してFreeモナドのfに束縛すればその欲しいモナドの抽象表現が得られる。

```haskell
data PutStrLnMonad a = PutStrLnMonadC String (() -> a)
```

ある関数`a0 -> a1 -> a2 -> ... aN -> m r`を持つモナドmを表現するようなFreeモナドのfは下のようになる。

```haskell
data M a = MC a0 a1 a2 ... aN (r -> a)
```

実のところ、fはFunctorであれば何でもよい。例えばfが`Const r`であれば、表現するモナドはEitherモナド...

```haskell
data M a = MC a0 a1 a2 ... aN (r0 -> a) (r1 -> a)
```

これは...モナドを...

複数の関数を持つモナドを表現するには単純に`|`で合わせればよい。

## Skeletonモナド

上のFreeモナドは関数を型に変換するのがめんどくさい。Skeletonモナドも同様に`Skeleton f a`という形式を持つが、関数から型への変換方法が簡単。
ある関数`a0 -> a1 -> a2 -> ... aN -> m r`を持つモナドmを表現するようなSkeletonモナドのfは下のようになる。

```haskell
data M a where
 MC :: a0 -> a1 -> a2 -> ... M r
```

GADTs拡張を使用。非常に簡単なのでFreeモナドではなくSkeletonモナドを使いたい。欠点はGADTsが難しいことぐらいしかないかもしれない。
