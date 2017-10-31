# 畳み込み

Haskellは`foldr`という関数を持つ。
できれば明示的な再帰より`foldr`を使えと言っているところもある。
一般的に弱い方法でできるのならそれを使ったほうがいいからだ。

## 動作についての疑問

```haskell
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys) = case x == y of
 False -> elem x ys
 True-> True

onceDelete :: Eq a => a -> [a] -> [a]
onceDelete _ [] = []
onceDelete x (y : ys) = case x == y of
 False -> y : onceDelete x ys
 True -> ys
```

全てを走査する場合は分かりやすい。しかし、
こんな感じの畳み込みを途中でやめるというパターンを`foldr`で表せるのか？

```haskell
elem :: Eq a => a -> [a] -> Bool
elem x = foldr go False where
 go y s = case x == y of
  False -> s
  True -> True

onceDelete :: Eq a => a -> [a] -> [a]
onceDelete x = fst . foldr go [] where
 go y (s0, s1) = (
  case x == y of
   False -> y : s0
   True -> s1,
  y : s1)
```

`elem`は分かりやすい。遅延評価があるので比較回数も`foldr`を使わない定義と同じである。
ちなみに使う関数の力を最小限にすると`any (x ==)`となる。しかし、この`onceDelete`は複雑すぎる。これが最良なのか？

## Beautiful Folding

[folds](https://hackage.haskell.org/package/folds)というパッケージがある。

## Recursion schemes

[recursion-schemes](https://hackage.haskell.org/package/recursion-schemes)というパッケージがある。
普通の畳み込みを一般化するだけではなく、畳み込み途中の情報を記録できるらしい。catamorphism、anamorphism、hylomorphism、histomorphismなど。

## extensible

recursion-schemesとextensibleを組み合わせると拡張可能な再帰が書ける。

`Fix f -> f (Fix f)`は畳み込みのパターンの基礎となる。ここで、fは`* -> *`のカインドを持つ型の拡張可能な直和とする。extensibleライブラリには`Instruction`という名前で含まれている。

fの拡張としてgがあるとする。つまりある型hが存在して`f ++ h ≡ g`である。この時、fに関する再帰`f a -> a`とhに関する再帰`h a -> a`からgに関する再帰`g a -> a`を構成できる。これは`(a -> c) -> (b -> c) -> Either a b -> c`の一般化である。

これが普通の拡張可能な再帰である。しかし、二つの型や三つの型が相互に再帰している場合もある。

aとbという型があり、aの中でaとbが使われていて、bの中でaとbが使われているとする。`a ≡ f a b`となるfと`b ≡ g b a`となるgを取る。

```haskell
data Fix2 f g = Fix2 (f (Fix2 f g) (Fix2 g f))
```

これを三つにも拡張する。

```haskell
data Fix3 f g h = Fix3 (f (Fix3 f g h) (Fix3 g h f) (Fix3 h f g))
```

二つの場合の再帰のタネは`(f a b -> a, g b a -> b)`である。fとgが拡張可能な直和であれば、同様に拡張可能な再帰となる。
