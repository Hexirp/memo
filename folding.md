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
