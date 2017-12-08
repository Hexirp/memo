# matching

依存型を使った時に、依存型なしでのプログラミングをする時には出てこない厄介な問題がある。matchをする時、`forall n, P n`のnに対してmatchすることだ。この時、matchすることによりnがOで構築されているかSで構築されているのか分かるのだから`match n with O => (* P O *) | S np => (* P (S np) *) end`となるべきである。しかしながら、普通の`match ... with ... end`ではこれを表せない。このようにする。

```coq
match n as n' return P n' with
| O => (* P O *)
| S np => (* P (S np) *)
end
```

もう一つ、依存型絡みではないが厄介な問題がある。

```coq
Inductive sig : nat -> Type :=
| sig_o : sig O
| sig_s : forall n, sig n -> sig (S n).
```

こんな感じの型が定義されているとする。この時、`forall n, sig n -> P n`の`sig n`に対してmatchすることだ。これにより、`sig_o`で構築されていることが分かればnがOであることが分かり、`sig_s`で構築されていることが分かればnが何らかの数npを取りS npであることが分かるはずだ。よって`match nsig with sig_o => (* P O *) | sig_s np nsigp => (* P (S np) *)`と書けるはずだ。しかし、普通の`match ... with ... end`ではこれを表せない。このようにする。

```coq
match nsig in sig n' return P n' with
| sig_o => (* P O *)
| sig_n np nsigp => (* P (S np) *)
end
```

これらは一般的に、matchすることにより型が詳細になる場合とすることが出来る。`as`キーワードや`in`キーワードで詳細になる部分がどんな名前で書かれるのか指定し、`return`キーワードの後で指定した名前を使って詳細になる所を指定する。詳細にできる部分全てを詳細になる所として指定しなければならないわけではなく、部分的に変えることもできる。これを使った証明テクニックの一つが`inversion`として呼ばれる物だ。

上の`in`キーワードを使ったmatchの時、`forall n, sig n -> P n`というものを例として出した。しかし、これが`forall n, (2 <= n) -> sig n -> P n`となっている場合を考えよう。普通にやると全体の式はこのようになる。

```coq
fun n H nsig => match nsig in sig n' return P n' with
| sig_o => (* P O *)
| sig_n np nsigp => (* P (S np) *)
end
```

この時、`2 <= n`という事実を使って`P n`を導きたいとする。特に`sig_o`にmatchした場合では矛盾を導くことが出来る...いや、こう書いたときは`P n`がmatchにより`P n'`に書き換えられるが、`2 <= n`が`2 <= n'`に書き換えられたりはしない。解決策としてこうすることが出来る。

```coq
fun n H nsig => match nsig in sig n' return n = n' -> P n' with
| sig_o => (* n = O -> P O *)
| sig_n np nsigp => (* n = S np -> P (S np) *)
end (eq_refl n)
```

`n = n'`を使ってnとn'が等しいことを保存する仕掛けだ。`n' = n'`ではない。matchの外部からはn'はnとして見えるので、`eq_refl n`を渡せばよい。これをタクティックとして行ってさらに等式を整理してくれるのが`inversion`コマンドである。
