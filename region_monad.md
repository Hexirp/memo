# region\_monad

Region モナドの実装、いや型付けを検討する。

```haskell
newVal :: a -> Region ctx (s ': ctx) (Val s a)
```

線形論理をベースとしないと駄目だ。

```haskell
id :: Region ctx a ctx a
(>>>) :: Region ctx0 a ctx1 b -> Region ctx1 b ctx2 c -> Region ctx0 a ctx2 c
```

さらに、こうする。

```haskell
embed :: (a -> b) -> Region ctx (Copies a) ctx b
embedVal :: a -> Region ctx () ctx (Copies a)
```

ここで `Copies` は、外側から持ち込まれた Haskell の型を表現するとともに、線形論理の `!` を表す。

```haskell
dropCopies :: Region ctx (Copies a) ctx ()
copyCopies :: Region ctx (Copies a) ctx (a, Copies a)
```

```haskell
runPtr :: Region ctx (a, forall s. Region (s ': ctx) (Ptr s a) (s ': ctx) b) ctx b
```

こんな感じだろうか？
