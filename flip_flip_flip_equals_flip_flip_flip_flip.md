# ちょっとした話

```
flip flip flip ≡ flip flip flip flip
```

なぜ？

## 証明1

```
flip :: (a -> b -> c) -> b -> a -> c
```

```
flip      :: (a             -> b -> c     ) -> b -> a             -> c     (_1)
     flip :: ((a -> b -> c) -> b -> a -> c)

a_1 = a -> b -> c
b_1 = b
c_1 = a -> c

flip flip ::                                   b -> (a -> b -> c) -> a -> c
```

```
flip flip      :: b                              -> (a   -> b                              -> c  ) -> a   -> c   (_1)
          flip :: ((a -> b -> c) -> b -> a -> c)

b_1 = (a -> b -> c) -> b -> a -> c

flip flip flip ::                                   (a_1 -> ((a -> b -> c) -> b -> a -> c) -> c_1) -> a_1 -> c_1
```

```
flip flip flip      :: (a_1           -> ((a -> b -> c) -> b -> a -> c) -> c_1   ) -> a_1                                                    -> c_1    (_2)
               flip :: ((a -> b -> c) -> b                              -> a -> c)

a_1_2 = a -> b -> c
b     = (a_2 -> b_2 -> c_2) -> b_2 -> a_2 -> c_2
c_1_2 = a -> c

flip flip flip flip ::                                                                (a -> ((a_2 -> b_2 -> c_2) -> b_2 -> a_2 -> c_2) -> c) -> a -> c
```

`flip flip flip`と`flip flip flip flip`は同じ型`(a' -> ((a -> b -> c) -> b -> a -> c) -> c') -> a' -> c'`を持つ。簡単な考察でこの型の値は一つしかないことが分かる。

## 証明2

```
flip x    y    z    = x    z    y

x = flip
y = flip
z = flip

flip flip flip flip = flip flip flip
```
