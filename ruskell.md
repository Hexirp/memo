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

move :: forall s p a b. Moveable p => (forall s0. s0 < s => Reg s0 (p s0 a)) ->. (forall s1. s1 < s => p s1 a ->. Reg s1 b) ->. Reg s b
```

## Val

```haskell
type Val :: LifeTime -> Type -> Type

mkVal :: forall s a. Sized a => a ->. Reg s (Val s a)
dcVal :: forall s a. Val s a ->. Reg s a

instance Moveable (Val s a)
```

Example:

```haskell
f :: Int ->. Int

main :: forall s. Reg s Int
main = move @s @Int @Int main0 main1

main0 :: forall s. Reg s (Val s Int
main0 = mkVal @s @Int 0
