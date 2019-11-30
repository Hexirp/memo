# import\_sentence

cllthask においての import 文を考える。

モジュールのシステムは、普通の直観主義論理に似たものにする。

## where 文の中での import 文

where 文の中には順序がない。それは module についた where 文でも、関数についた where 文でも同じである。

その中に import 文を書けるようにする。

## let 式の中での import 文

`let { import Foo } in ...` の `...` の中で `Foo` の要素を使うことを許すべきか。

許すと do 式の中での import 文が実装できる。すなわち `do { import M; ... }` を `let { import M } in ...` に変換できる。
