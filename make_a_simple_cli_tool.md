# make a simple cli tool

yhseq 、バージョン 3.0 を実装するのはとても困難な道になりそうなので、先にコマンド部分を作成することにした。

optparse-applicative 始めた。 Hackage の英語を読めば大体理解できる。初めは long で長い方を short で短い方を metavar で変数を help で説明を、それらをモノイドで合成して関数を掛けてオプションを作成。

option の順序は関係ない。そのために applicative を使っている。並行にできるから。モナドではダメ。

alternative も使える。両立しないオプションに使える。

なぜか Parser を ParserInfo に変えなければならない。

オプションのビルダーにも色々ある。普通は文字列になる。オプションがあるかどうかのプール型もある。また option auto で read 型クラスを使える。

フラグも使える。二つの値を渡して、どちらかの選択。これは switch つまり前の段落と同じみたいのになる。あと many と組み合わせてオプションの数を、数えることもできる。

引数ももちろんある。あと some や many と組み合わせて不定数も。あと -- の後は必ず引数でなければならない。

特に注目なのが command で、サブコマンドを司る。

https://hackage.haskell.org/package/optparse-applicative-0.15.1.0

読んだのは、ここの Builder の最後まで
