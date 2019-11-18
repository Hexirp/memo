# Data on Coq

本当は複雑な実装を要するデータ型を Coq でずるして実装してみましょう。

## Set

```
Set : Type -> Type
Set a = a -> Bool

Set : Type -> Type
Set a = sigma (x : List a), pi e, IsHProp (Elem e x)
```

## Map

```
Map :: Type -> Type -> Type
Map k v = k -> Maybe v

Map :: Type -> Type -> Type
Map k v = Set k * (k -> Maybe v)

Map :: Type -> Type -> Type
Map k v = sigma (x : List (k * v)), pi ka, IsHProp (sigma va, Elem (ka * va) x)
```

## Record

```
Record : (k : Type) -> Map k Type -> Type
Record k kv = pi k', pi v', kv k' :~: Just v' -> v'

Variant : (k : Type) -> Map k Type -> Type
Variant k kv = sigma k', pi v', kv k' :~: Just v' -> v'
```

## Graph

```
Graph : Type -> Type
Graph a = a -> a -> Type
```

トポロジカルソートは

```
sort : [a] -> Graph a -> [a]
     : [a] -> (a -> a -> Bool) -> [a]
```
