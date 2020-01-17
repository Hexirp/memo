# Haskell 黒魔術大全

## tehepero

> Rustのexpectとunwrapが便利なのでHaskellに移植した。カジュアルに実行時エラーを出したいときのeither error idやeither fail pureなど、あまり行儀のよろしくないコードを減らせるはずだ
> https://github.com/fumieval/tehepero/blob/632850766ac79615f969ae6861d7d0416369f03e/src/Control/Panic.hs
> -- https://twitter.com/fumieval/status/1217801501357133826

fallible というパッケージのクラスを使ってるのか

Faillible は Either の簡単な一般化で、ここでは型クラスだけ使っていて、継続モナドによる例外処理は使っていない。

case x of { Left e -> print e; Right x -> k x } みたいに継続 k をエラー処理の分岐にはめ込んで平らにするみたいなパッケージ

関数は either error id を高級な道具を使ってベストになるように書いてあるみたい

## tagswep

> HTMLスクレイパーライブラリのtagsoupは遅いとよく言われているので、オートマトンを直書きした上に黒魔術をモリモリ使った高速な実装を作った。少なくとも3倍程度は速いようだ
> https://github.com/fumieval/tagstew/tree/facdd55981bab07de1d865c691a7c6c05bc975b2
> -- https://twitter.com/fumieval/status/1217802607957176321

PatternSynonyms を使って XXXXYYYY みたいに一つのバイト列に二つのデータが一緒に入っているのを分解するのを普通のパターンのように扱う、

並行に実行されるステートマシンの状態の記録（？）に MVar で同期を取る、それを隠蔽するために unsafePerformIO を使う、

（ MVar なので実行の順序は問題なく、一つの関数の中でしか使われていないし、最終的に MVar を除いて外部に作用を及ぼさないので問題ない）

リストの遅延評価を再現するために unsafe (a : unsafe (b : ...)) のような構造を再帰で作り、コンパイル時の何かを制御するために () を引数に受け取っている

黒魔術だ……

> tagstewでは、HTMLのエンティティ(\&amp;など)を引くテーブルがそこそこ大きいのでCompact regionに納めています。GCに費やす時間をおよそ2割削減できました
> https://github.com/fumieval/tagstew/blob/facdd55981bab07de1d865c691a7c6c05bc975b2/Text/HTML/TagStew/Entity.hs
> -- https://haskell-jp.slack.com/archives/C4M4TT8JJ/p1579249585004500
