# ReaderT IO pattern vs Extensible Effect

なぜ RIO が使われて Extensible Effect が使われないのか。

RIO は通常の Haskell の形式の拡張であり、型レベルリストなどを必要としない。

RIO は IO がベースだと決めているため様々なものがただで手に入る。

RIO は作用を書く時に lift などの操作を必要としない。

RIO は実行する時に順序を考える必要がない。

RIO は実行する部分を作る時に普通の関数を定義するのと同じようにすればよい。

RIO はどのハンドラーで実行するか考える手間がない。いや、どのモナドでだ。

このような細々とした不便さの集まり。


Extensible Effect が使われるためには。

IO をベースにする…が、これでは RIO と同じである。勝っているのは純粋に実行できる、これしかない。

State env + Expect e 辺りをベースにして、 env には RIO と同様の Has 型クラスパターンを使う。

純粋に実行するための型クラスでも作っておく。それで、型レベルの計算、たとえば ForAll とか使っておけば一斉に実行する関数を作れる。

```haskell
class HasPurely m where
  type Env m
  type Error m
  run :: (Has (Env m) env, Include (Error m) e) => Skeleton m a -> ExState env e a

runs :: (ForAll (\x -> HasEnv x env) eff, ForAll (\x -> IncludeError x e) eff) => Effect eff a -> ExState env e a
```
