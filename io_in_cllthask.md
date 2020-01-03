# io in cllthask

  A * RealWorld -> B * RealWorld
  A * RealWorld -> (B + E) * RealWorld

これが IO モナドを表現するもの。なお _ * _ は乗法的論理積であり _ + _ は加法的論理和であり _ -> _ は線形含意である。

一意型は (forall s1. s1 < s0 -> Ref s1 a -> Reg s1 b) -> Reg s0 b という型である意味表現できる。

IO e a b は e と b に関して関手であり a に関して反変関手である。

throwM :: IO e e a

catch :: IO e 1 a -> IO e e a -> IO e 1 a

mask :: ((forall a. IO e 1 a -> IO e 1 a) -> IO e 1 b) -> IO e 1 b

ここで mask から ((forall a b. (b -> IO a) -> b -> IO a) -> c -> IO d) -> c -> IO d という関数を作れるので、このような一般化をできる。

catch :: IO e a b -> IO e e b -> IO e a b

mask :: ((forall a b. IO e a b -> IO e a b) -> IO e c d) -> IO e c d

RealWorld の値は不確定的である。その値は純粋ではない。

RealWorld は常に正格評価される。すなわち、 RealWorld -> a という関数は左からカット除去される。いや、正格モードなら普通に、反正格モードなら常に case 式で分解されているかのように評価される。

return :: IO e a a などの基本的操作も RealWorld の状態によっては例外が発生しうる。つまり、非同期的例外である。

あるいは、 RealWorld の中にランダムな値が入っているという解釈でもよい。非純粋に見えても、与えられた値に非常に細かな違いがあるという考え方である。いずれにせよ、 RealWorld の値は意味論的にも明示的に扱うことができない。
