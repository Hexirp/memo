# 畳み込み

Haskellは`foldr`という関数を持つ。
明示的な再帰より`foldr`を使えと言っているところもある。
一般的に弱い方法でできるのならそれを使ったほうがいいからだそうだ。

## 動作についての疑問

```haskell
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys) = if x == y
 then True
 else elem x ys

onceDelete :: Eq a => a -> [a] -> [a]
onceDelete _ [] = []
onceDelete x (y : ys) = if x == y
 then ys
 else y : onceDelete x ys
```

こんな感じの畳み込みを途中でやめるというパターンを`foldr`で表せるのか？
