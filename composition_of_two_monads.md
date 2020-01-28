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

```haskell
tailRecM :: forall r a b. a -> (a -> Cont r (Either a b)) -> Cont r b
tailRec x f = go x where
  go :: a -> Cont r b
  go = join . fmap (either go pure) . f

tailRecM :: forall r a b. a -> (a -> Cont r (Either a b)) -> Cont r b
tailRec x f = go x id where
  go :: forall r'. ((a -> Cont r b) -> r') -> r'
  go k = k (join . fmap (either (go _) pure) . f)

tailRecM :: forall r a b. a -> (a -> Cont r (Either a b)) -> Cont r b
tailRec x f = go x id where
  f' :: (a -> Cont r b) -> a -> Cont r b
  f' g = join . fmap (either g pure) . f
  go :: forall r'. ((a -> Cont r b) -> r') -> r'
  go k = k (f' (go _))

tailRecM :: forall r a b. a -> (a -> Cont r (Either a b)) -> Cont r b
tailRec x f = go x id where
  f' :: (a -> Cont r b) -> a -> Cont r b
  f' g = join . fmap (either g pure) . f
  go :: forall r'. ((a -> Cont r b) -> r') -> r'
  go k = go (k . f')
```

無理やり継続にして末尾再帰にした。できたけど、無理やりすぎる。

```haskell
tailRecM :: forall r a b. a -> (a -> Cont r (Either a b)) -> Cont r b
tailRec x f = go x id where
  f' :: (a -> Cont r b) -> a -> Cont r b
  f' g = join . fmap (either g pure) . f
  f' g = \x -> Cont $ \k -> runCont (f x) $ \xe -> case xe of
    Left xa -> runCont (g xa) k
    Right xb -> k xb
  go :: forall r'. ((a -> Cont r b) -> r') -> r'
  go k = go (k . f')
```

ミックスしてこうやればちったあましになるか？

### モナド変換子

```haskell
tailRecM :: forall m. MonadRec m => forall a b. a -> (a -> IdentityT m (Either a b)) -> IdentityT m b
tailRecM x f = IdentityT $ tailRecM x (runIdentityT . f)

tailRecM :: forall m. MonadRec m => forall a b. a -> (a -> MaybeT m (Either a b)) -> MaybeT m b
tailRecM x f = go x where
  go :: a -> MaybeT m b
  go = join . fmap (either go pure) . f
  go x = join (fmap (either go pure) (f x))
  go x = (fmap join . join . fmap sequenceA . runMaybeT . fmap runMaybeT) (fmap (either go pure) (f x))
```

一段と難しくなった。

```haskell
tailRecM :: forall m. MonadRec m => forall a b. a -> (a -> MaybeT m (Either a b)) -> MaybeT m b
tailRecM x f = MaybeT $ tailRecM x $ \y -> flip fmap (runMaybeT $ f y) $ \ye -> undefined

tailRecM :: forall m. MonadRec m => forall a b. a -> (a -> MaybeT m (Either a b)) -> MaybeT m b
tailRecM x f = MaybeT $ tailRecM x loop where
  loop :: a -> m (Either a (Maybe b))
  loop x = fmap trans (runMaybeT $ f x)
  trans :: Maybe (Either a b) -> Either a (Maybe b)
  trans me = case me of
    Nothing -> Right Nothing
    Just e -> case e of
      Left ea -> Left ea
      Right eb -> Right (Just eb)
```

https://github.com/typelevel/cats/blob/93ebcedd9486b8e512d2e88782c30914fa5c29c8/core/src/main/scala/cats/data/OptionT.scala#L423-L430

またもや別実装を参考にした。

https://github.com/typelevel/cats/blob/93ebcedd9486b8e512d2e88782c30914fa5c29c8/core/src/main/scala/cats/data/OptionT.scala#L17-L18

`fold` がどういう関数か分からなくて悩んだ。初めは `fold :: (Foldable f, Monoid a) => f a -> a` っていう関数だと思っていたけど結局これ。

```haskell
tailRecM :: forall m. MonadRec m => forall a b. a -> (a -> MaybeT m (Either a b)) -> MaybeT m b
tailRecM x f = MaybeT $ tailRecM x loop where
  loop :: a -> m (Either a (Maybe b))
  loop x = fmap trans (runMaybeT $ f x)
  trans :: Maybe (Either a b) -> Either a (Maybe b)
  trans = sequenceA
```

すなわち、こう。まさか `Maybe :.: m ~> m :.: Maybe` のためにではなく `Maybe :.: Either a ~> Either a :.: Maybe` のために `sequenceA` を使うとは……。

そして、ここから、ぽんっと `Compose m n` の実装が出てくる。

```haskell
tailRecM :: forall m n. (MonadComm m, MonadRec m, Monad n, Traversable n) => forall a b. a -> (a -> Compose m n (Either a b)) -> Compose m n b
tailRecM x f = Compose $ tailRecM x (fmap sequenceA . getCompose . f)
```

これは Kory さんが考えた実装 ( [https://twitter.com/Kory__3/status/1221895873887719425](https://twitter.com/Kory__3/status/1221895873887719425) ) と一致……しない。

```haskell
tailRecM :: forall m r. MonadRec m => forall a b. a -> (a -> ContT r m (Either a b)) -> ContT r m b
tailRecM x f = ContT $ go x where
  go :: a -> (b -> m r) -> m r
  go x k = runContT (f x) $ \xe -> case xe of
    Left xa -> go xa k
    Right xb -> k xb

tailRecM :: forall m r. MonadRec m => forall a b. a -> (a -> ContT r m (Either a b)) -> ContT r m b
tailRecM x f = ContT $ go x where
  f' :: (a -> ContT r m b) -> a -> ContT r m b
  f' g = \x -> ContT $ \k -> runContT (f x) -> \xe -> case xe of
    Left xa -> runContT (g xa) k
    Right xb -> k xb
  go :: forall r'. ((a -> ContT r m b) -> r') -> r'
  go k = go (k . f')
```

`ContT` への実装。

あっ、 go が停止しないのに今ようやく気付いた。

```haskell
tailRecM :: forall m r. MonadRec m => forall a b. a -> (a -> ReaderT r m (Either a b)) -> ReaderT r m b
tailRecM x f = ReaderT $ \r -> tailRecM x (\x -> runReaderT (f x) r)
```

やっぱり `ReaderT` は全体的にお行儀が良い。

```haskell
tailRecM :: forall m w. (MonadRec m, Monoid w) => forall a b. a -> (a -> WriterT w m (Either a b)) -> WriterT w m b
tailRecM x f = undefined
```
