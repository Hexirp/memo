# monad-skeleton

monad-skeletonはSkeletonモナド、またはFreerモナド、またはOperationalモナドを提供する小さくまとまったライブラリ。

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
複数の関数を持つモナドを表現したいときはFreeモナドと同じようにする。

# 内部実装

実はSkeletonモナドはFreeモナドを基にしている。つまり、概念的にはこうなる。

```haskell
data Coyoneda f a where
 CoyonedaC :: f x -> (x -> a) -> Coyoneda f a

newtype Skeleton f a = SkeletonC (Free (Coyoneda f) a)
```

もちろん本当の内部実装はもっと最適化されていて次のようになる。

```haskell
data Skeleton t a where
 ReturnS :: a -> Skeleton t a
 BindS :: t a -> Cat (Kleisli (Skeleton t)) a b -> Skeleton t b
```

なぜこうなるのか？まず最初の実装はFreeをラップしているのでSkeletonへ融合させる。

```haskell
data Coyoneda f a where
 CoyonedaC :: (x -> a) -> f a -> Coyoneda f a

data Skeleton f a = ReturnS a | BindS (Coyoneda f (Skeleton f a))
```

CoyonedaをSkeletonに融合させる。

```haskell
data Skeleton f a where
 ReturnS :: a -> Skeleton f a
 BindS :: f x -> (x -> Skeleton f a) -> Skeleton f a
```

ここで、Skeleton型の値がどのようになるかを考える。`BindS`が積み重なったものになる。

```haskell
BindS (BindS (BindS ...(BindS (ReturnS a) fN)... f2) f1) f0
```

ここで値が0個以上の関数と一つの値で構成されていることに注目する。つまり、

```haskell
data Skeleton f a where
 ReturnS :: a -> Skeleton f a
 BindS :: f x0 -> (x0 -> Skeleton f x1) -> (x1 -> Skeleton f x2) -> (x2 -> Skeleton f x3) -> ... (xN -> Skeleton f a) -> Skeleton f a
```

`_ -> Skeleton f _`を`Kleisli (Skeleton f)`に変換すると、

```haskell
data Skeleton f a where
 ReturnS :: a -> Skeleton f a
 BindS :: f x0 -> Kleisli (Skeleton f) x0 x1 -> Kleisli (Skeleton f) x1 x2 -> Kleisli (Skeleton f) x2 x3 -> ... Kleisli (Skeleton f) xN a -> Skeleton f a
```

これはCategoryを思い浮かべさせる。

```
class Category cat where
 (>>>) :: cat a b -> cat b c -> cat a b
```

この関数の列を表現したい。

```
data Cat k a b where
 Leaf :: k a b -> Cat k a b
 Tree :: Cat k a b -> Cat k b c -> Cat k a c
```
