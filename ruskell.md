# ruskell

Rust の基本的な操作を Haskell で型安全に記述することを考えたら、なぜかどんどん現実味を帯びてきたのでここに書き込みます。

## Region Monad

```haskell
type LifeTime :: Type

type (<) :: LifeTime -> LifeTime -> Constraint

type Reg :: LifeTime -> Type -> Type

bind :: forall s a b. Reg s a ->. (a ->. Reg s b) ->. Reg s b

return :: forall s a. a ->. Reg s a
```

## Val

```haskell
type Val :: LifeTime -> Type -> Type

mkVal :: forall s a. Sized a => a ->. Reg s (Val s a)
dcVal :: forall s a. Val s a ->. Reg s a

mvVal :: forall s a b. (forall s0. s0 < s => Reg s0 (Val s0 a)) ->. (forall s1. s1 < s => Val s1 a ->. Reg s1 b) -> Reg s b
```

Example:

```haskell
f :: Int ->. Int

main :: forall s. Reg s Int
main = mkVal 0 `bind` \v -> dcVal v `bind \n -> f n
```
