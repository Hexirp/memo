# matching

依存型を使った時に、依存型なしでのプログラミングをする時には出てこない厄介な問題がある。matchをする時、`forall n, P n`のnに対してmatchすることだ。この時、`match n with O => (* P O *) | S n => (* P (S n) *) end`となるべきである。しかしながら、普通の`match ... with ... end`ではこれを表せない。このようにする。

```coq
match n as n’ return P n’ with
  O => (* P O *)
  S n => (* P (S n) *)
end
```
