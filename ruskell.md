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

## Move

```haskell
type Moveable :: Type -> Constraint

move :: forall s a b. Moveable a => (forall s0. s0 < s => Reg s0 a) ->. (forall s1. s1 < s => a ->. Reg s1 b) ->. Reg s b
```

## Val

```haskell
type Val :: Type -> Type

mkVal :: forall s a. Sized a => a ->. Reg s (Val a)
dcVal :: forall s a. Val a ->. Reg s a

instance Moveable
```

Example:

```haskell
f :: Int ->. Int

main :: forall s. Reg s Int
main = mkVal 0 `bind` \v -> dcVal v `bind \n -> f n
```
