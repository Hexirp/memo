# モナドの合成と色々

モナドの合成と MonadComm と MonadFix と tailRecM について。

## Monad (Compose m n)

ストリング図を使ってモナドの合成に関する計算をした。やはり `instance (MonadComm m, Monad n, Traversable n) => Monad (Compose m n)` がファイナルアンサー感がある。

* どこかで `t :: Compose n m ~> Compose m n` が必要になる
* モナド則を満たすためには `t` がある特定の条件を満たさなければならないが、そうなるとそれは `sequenceA @n @m` と一致するというのには一定の説得力がある

こんな型クラスを使う。

```haskell
class Monad m => MonadComm m where
  -- f <$> x <*> y === flip f <$> y <*> x
```

## MonadFix と tailRecM

```haskell
mfix :: MonadFix m => (a -> m a) -> m a

tailRecM :: forall m a b. Monad m => a -> (a -> m (Either a b)) -> m b
tailRecM x f = go x where
  go :: a -> m b
  go x = do
    e <- f x
    case e of
      Left a -> go a
      Right b -> return b
  go x = f x >>= \case
    Left a -> go x
    Right b -> return b
  go x = join $ fmap (either go pure) $ f x
  go = join . fmap (either go pure) . f

mfix’ :: MonadFix m => (a -> b -> m a) -> b -> m a
mfix’ f = \b -> mfix (\a -> f a b)
mfix’ f = runReaderT $ mfix (\a -> ReaderT $ f a)

mfix :: forall m a. Monad m => (a -> m a) -> m a
mfix f = go where
  go :: m a
  go = do
    a <- go
    f a
  go = go >>= f
  go = join $ fmap f go
  go = (join . fmap f) go
mfix f = fix (join . fmap f)

mfix :: MonadRec m => (a -> m a) -> m a
mfix f = tailRecM undefined (fmap Left . f)
```

この実装は間違っている。

mfix と tailRecM は互いに実装し合うことができない。

```haskell
mfix :: (a -> Maybe a) -> Maybe a
mfix f = fix (join . fmap f)
```

この実装はナイーブなものである。

```haskell
fix (join . fmap f)

join $ fmap f $ fix (join . fmap f)

case f <$> fix (join . fmap f) of
  Nothing -> Nothing
  Just ma -> ma

case fix (join . fmap f) of
  Nothing -> Nothing
  Just a -> case f a of
    Nothing -> Nothing
    Just ma -> ma
```

ここまで来れば分かるが、延々と評価が続き f の正格性に関係なく評価が終わらない。

MonadFix の IO へのインスタンスは unsafeDupableInterleaveIO という妙な関数と MVar を使っている。なぜだろうかと思っていたけど、納得できた。

k を結えたい関数とする。 MVar から値を取り出す操作を遅延して、それを k に渡して MVar にその結果を格納する。つまり、 MVar を通して結び目を作っている。

取り出す操作を遅延 IO 化するというのがポイントで、これにより作用が行われないまま、取り出した値を先に使用した結果を取り出そうとしている参照に格納することができる。結び目ができる。

しかし、こういう unsafe な何かに MVar ってよく使われるよなあ。


話を戻す。 Maybe においては、正格性に関係なく途中の計算で Nothing が現れたら全ての計算が Nothing になる。この性質は mfix の実装に於いては都合が悪い。無限の適用の列において、その無限の先の全てまで Nothing でないことを確かめないと Just _ を返せないからだ。

そこで Nothing でないことを仮定してしまおう。


```haskell
tailRecM :: forall a b. a -> (a -> Maybe (Either a b)) -> Maybe b
tailRecM x f = go x where
  go :: a -> Maybe b
  go = join . fmap (either go pure) . f
```

これは、遅延評価においては正格性にかかわらずループしてしまうことはない。しかし、正格評価では期待する効果が得られない。それは either による分岐で pure が得られるまで join がスタックに積み重なり続けてしまうからである。


```haskell
tailRecM :: forall a b. a -> (a -> Maybe (Either a b)) -> Maybe b
tailRecM x f = go x where
  go :: a -> Maybe b
  go x = x `seq` case f x of
    Nothing -> Nothing
    Just e -> case e of
      Left a -> go a
      Right b -> Just b
```

これは元々の定義の関数をインライン展開した後、ネストしている case 文を平坦にしただけである。


```haskell
tailRecM :: forall a b. a -> (a -> Identity (Either a b)) -> Identity b
tailRecM x f = go x where
  go :: a -> Identity b
  go x = x `seq` case f x of
    Identity e -> case e of
      Left a -> go a
      Right b -> b

tailRecM :: forall a b. a -> (a -> r -> Either a b) -> r -> b
tailRecM x f = go x where
  go :: a -> r -> b
  go x = x `seq` \r -> case f x r of
    Left a -> go a r
    Right b -> b

tailRecM :: forall a b. a -> (a -> [Either a b]) -> [b]
tailRecM x f = go x where
  go :: a -> [b]
  go x = undefined
```

この実装の形式では concatMap などを使わざるを得ない。それではナイーブな実装と同じになる。

```haskell
tailRecM :: forall a b. a -> (a -> [Either a b]) -> [b]
tailRecM x f = go id [Left x] where
  go :: ([b] -> [b]) -> [Either a b] -> [b]
  go k x = k `seq` x `seq` case x of
    [] -> k []
    xv : xs -> case xv of
      Left xa -> go k (f xa ++ xs)
      Right xb -> go (k . (xb :)) xs
```

https://github.com/typelevel/cats/blob/66a717453c5faaf618528e052f454e2a57047247/core/src/main/scala/cats/instances/list.scala

これを参考にした。……しかし、元々ではどうやっているんだろうかと思ったら、まさか非純粋な計算を行なっているとは思わなかったよ。

tailRecM はくそなのでは？

ここでは純粋に書き換えた。問題はないと思う。二重リストを使う代わりに (++) を使っているけど、この場合では効率には大した影響は多分ない。

```haskell
tailRecM :: forall a b. a -> (a -> [Either a b]) -> [b]
tailRecM x f = go id [Left x] where
  go :: ([b] -> [b]) -> [Either a b] -> [b]
  go k x = k `seq` case x of
    [] -> k []
    xv : xs -> case xv of
      Left xa -> xa `seq` go k (f xa ++ xs)
      Right xb -> xb `seq` go (k . (xb :)) xs
```

評価戦略をいじって a に関して正格になるようにしてみた。

```haskell
tailRecM :: forall a b. a -> (a -> [Either a b]) -> [b]
tailRecM x f = go id [Left x] where
  go :: ([b] -> [b]) -> [Either a b] -> [b]
  go k x = case x of
    [] -> k []
    xv : xs -> case xv of
      Left xa -> xa `seq` go k (f xa ++ xs)
      Right xb -> go (k . (xb :)) xs
```

こっちの方がいいか？

[Left x] のところはベースにすべきものが分からなくて一瞬 (f x) にしようかと思った。見つけられたわたしは天才。


```haskell
tailRecM :: forall w. Monoid w => forall a b. a -> (a -> (Either a b, w)) -> (b, w)
tailRecM x f = go x where
  go :: a -> (b, w)
  go x = x `seq` case f x of
    (xe, w) -> case xe of
      Left xa -> case go xa of
        (xa’, w’) -> (xa’, w <> w’)
      Right xb -> b
```

だめだ。これじゃ末尾再帰にならない。

```haskell
tailRecM :: forall w. Monoid w => forall a b. a -> (a -> (Either a b, w)) -> (b, w)
tailRecM x f = go x mempty where
  go :: a -> w -> (b, w)
  go x w = x `seq` case f x of
    (xe, w’) -> case xe of
      Left xa -> go xa (w <> w’)
      Right xb -> (b, w <> w’)
```

これは Strict Writer だ。

```haskell
tailRecM :: forall w. Monoid w => forall a b. a -> (a -> (Either a b, w)) -> (b, w)
tailRecM x f = go x mempty where
  go :: a -> w -> (b, w)
  go x w = x `seq` case f x of
    ~(xe, w’) -> case xe of
      Left xa -> go xa (w <> w’)
      Right xb -> (b, w <> w’)
```

これが Lazy Writer になる。だが、両者の区別はないと思う。

```haskell
tailRecM :: forall s a b. a -> (a -> State s (Either a b)) -> State s b
tailRecM x f = State $ \s -> go x s where
  go :: a -> s -> (b, s)
  go x s = x `seq` case runState (f x) s of
    (xe, s’) -> case e of
      Left xa -> go xa s’
      Right xb -> (b, s’)
```

例によって Strict State である。

```haskell
tailRecM :: forall r a b. a -> (a -> Cont r (Either a b)) -> Cont r b
tailRecM x f = Cont $ go x where
  go :: a -> (b -> r) -> r
  go x k = runCont (f x) $ \xe -> case xe of
    Left xa -> go xa k
    Right xb -> k xb
```

tailRecM はくそ。これじゃ末尾再帰にはならないなあ。

```haskell
tailRecM :: forall r a b. a -> (a -> Cont r (Either a b)) -> Cont r b
tailRecM x f = Cont $ go0 x where
  go0 :: a -> (b -> r) -> r
  go0 x k = go1 x where
    go1 :: a -> r
    go1 x = runCont (f x) $ \xe -> case xe of
      Left xa -> go xa
      Right xb -> k xb
```

https://github.com/typelevel/cats/blob/825346243746090eb46935c8189ed9ab30576d4b/core/src/main/scala/cats/data/ContT.scala

cats での定義を翻訳してみた。

はい、終了ーー！！！

まあ、最初から Cont は末尾再帰みたいなもんだからあれだけど。

```haskell
tailRecM :: forall r a b. a -> (a -> Cont r (Either a b)) -> Cont r b
tailRecM x f = Cont undefined where
  go :: a -> (Either a b -> r) -> r
  go x k = undefined
```

くそ。

```haskell
f 0 = 1
f n = n * f (n - 1)

f 0 k = k 1
f n k = f (n - 1) (k . (\x -> n * x))
```

同じように……

```haskell
tailRecM :: forall r a b. a -> (a -> Cont r (Either a b)) -> Cont r b
tailRecM x f = Cont $ go x where
  go :: a -> (b -> r) -> r
  go x k = runCont (f x) $ \xe -> case xe of
    Left xa -> go xa k
    Right xb -> k xb

tailRecM :: forall r a b. a -> (a -> Cont r (Either a b)) -> Cont r b
tailRecM x f = Cont $ go x where
  go :: a -> (b -> r) -> r
  go x k = runCont (f x) $ \xe -> case xe of
    Left xa -> go xa k
    Right xb -> k xb
```

だめだ。 xa は runCont を使って取り出すしかない。

何を状態にする？　と言ったら…… a か Either a b か…いや Cont r (Either a b) か。

```haskell
tailRecM :: forall r a b. a -> (a -> Cont r (Either a b)) -> Cont r b
tailRecM x f = Cont $ go (f x) where
  go :: Cont r (Either a b) -> (b -> r) -> r
  go fx k = runCont fx $ \xe -> case xe of
    Left xa -> go (f xa) k
    Right xb -> k xb
```

これでもだめなのか……いや？

```haskell
tailRecM :: forall r a b. a -> (a -> Cont r (Either a b)) -> Cont r b
tailRecM x f = Cont $ go (f x) where
  go :: Cont r (Either a b) -> (b -> r) -> r
  go fx = \k -> runCont fx $ \xe -> case xe of
    Left xa -> go (f xa) k
    Right xb -> k xb

tailRecM :: forall r a b. a -> (a -> Cont r (Either a b)) -> Cont r b
tailRecM x f = Cont $ go (f x) where
  go :: Cont r (Either a b) -> (b -> r) -> r
  go fx = \k -> runCont fx $ \xe -> case xe of
    Left xa -> go (f xa) k
    Right xb -> k xb
```

だめっぽいな。

```haskell
tailRecM :: forall r a b. a -> (a -> Cont r (Either a b)) -> Cont r b
tailRecM x f = Cont (go x) where
  go :: a -> (b -> r) -> r
  go x k = runCont (f x) (\xe -> case xe of { Left xa -> go xa k; Right xb -> k xb })
  go x k = ($ (\xe -> case xe of { Left xa -> go xa k; Right xb -> k xb })) (runCont (f x))
  go x k = ($ (\xe -> case xe of { Left xa -> go xa k; Right xb -> k xb })) (($ f x) runCont)
  go x k = ($ either (\xa -> go xa k) (\xb -> k xb)) (($ f x) runCont)
  go x k = ($ either (\xa -> go xa k) k)) (($ f x) runCont)
  go x k = ($ ($ k) (either (\xa -> go xa k)))) (($ f x) runCont)
  go x k = ($ ($ k) (($ \xa -> go xa k) either))) (($ f x) runCont)
  go x k = ($ ($ k) (($ flip go k) either))) (($ f x) runCont)
  go x k = ($ ($ k) (($ ($ k) flip go) either))) (($ f x) runCont)
  go x k = ($ ($ k) (($ ($ k) (($ go) flip)) either))) (($ f x) runCont)
  go x k = ($ ($ k) (($ ($ k) (($ go) flip)) either))) (($ ($ x) f) runCont)
  go x k = ($ ($ k) (($ ($ k) (($ go) flip)) either))) $ ($ ($ x) f) $ runCont
  go x k = (($ ($ k)) $ ($ ($ k) (($ go) flip)) $ either)) $ ($ ($ x) f) $ runCont
  go x k = go0 x k id where
    go0 x k k' = ($ ($ k) (($ ($ k) (($ go) flip)) either))) (($ ($ x) f) runCont)
```

無理やりCPS変換する。当然のようにだめ。
