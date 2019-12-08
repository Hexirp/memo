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

# Haskell で簡単な CLI ツールを作ってみる

Haskell Advent Calendar 2019 の八日目は「 Haskell で簡単な CLI ツールを作ってみる」です。

さて、このタイトルで内容は想像しづらいかと思いますが、この記事では内部の実装は既に終わっていると仮定しています。その上で、コマンドの引数はどのようにすれば受け取れるのか、オプションを作るためにはどうすればいいのか、簡単に紹介していく記事になります。

## Step 1

`foo :: [Integer] -> Integer -> [Integer]` という関数が用意されています。これを使って以下のようなコマンドを作ります。

```
$ foo-simple [1,2] 3
[1,1,1,1]

$ foo-simple [1,2,3] 3
[1,2,2,2,2]
```

つまり、与えられた二つの引数を `foo` に与えて、その返り値を表示するだけのコマンドです。引数の数が合わないときはエラーとしましょう。

`Prelude` モジュール以外の関数で使う必要があるのは一つだけです。それは、引数を取ってくる作用を持つもの `getArgs` です。この関数が含まれているモジュールは `System.Environment` です。なので、インポートリストに書いておきましょう。

```haskell
module Main where

  import Prelude
  import System.Environment (getArgs)

  import Foo.V0210 (foo)

  main :: IO ()
  main = do
    args <- getArgs
    print $ case args of
      [seq, num] -> foo (read seq) (read num)
      _ -> error "The arguments are incorrect!"
```

`getArgs` の型は `IO [String]` です。このプログラムをコンパイルして以下のように実行したとき、このプログラム内の `args` の値は以下のようになります。

```
./foo-simple                     ==> []
./foo-simple 1                   ==> ["1"]
./foo-simple 1 2                 ==> ["1","2"]
./foo-simple [1,2] 3             ==> ["[1,2]","3"]
./foo-simple "foo foo" 'baa baa' ==> ["foo foo","baa baa"]
```

単純にコマンドラインの引数ですね。

そして、得られたリストにパターンマッチします。この時、引数の数が合わなければエラーとします。合っていれば `foo` に引数を渡しますが、このとき文字列からそれぞれのあるべき型の値に変換する必要があります。それには、 `read` を使います。そして、 `foo` の値を `print` して終わりです。

注意しておくと、ここで `read` を使いましたが、この関数には罠があります。例えば、 `f x = show (read x)` と関数を定義したとき、コンパイルエラーになります。それは `read` でどのような型と見なして読むかコンパイラに判断できないからです。しかし、ここでは `foo` により既に型が確定していますので問題ありません。

ここまでは導入です。

## Step 2

オプションが色々欲しくなりました。まず、展開の仕方には実はバージョンがあります。今までの `foo` は v0.2.1.0 を使うと仮定していました。それは、 `foo` をインポートしている所から分かると思います。

そして、バージョンの中には非推奨になったものがあります。もし非推奨なバージョンが渡されたらエラーになるようにします。そして、非推奨なバージョンの使用を強制するオプション `-f`, `--force` も用意しておきたいとします。

次に、 `foo` の計算過程を詳細に表示するオプション `-d`, `--detail` も用意しておきたいとします。

最後に、いつもの `-h`, `--help` と `-v`, `--version` も用意しておきたいとします。

このようなコマンドを作りたいとします。仕様は以下のようになるでしょうか。

```
foo VERSION SEQ NUM
```

上の仕様を満たすようにナイーブな実装をしようとすると困難に直面することになります。引数を追加するまでは大丈夫です。ただ、途中の case 式でのパターンマッチングを `[version, seq, num]` に変えてよしなに処理を変えるだけです。

しかし、オプションも受け取れるようにすると、通常のパターンマッチングではとても不可能です。なぜならば、以下のような何通りものパターンが現れるからです。（ Egison ならナイーブに書けるのでしょうか……？）

```
foo v0.1.0.0 -f [1,2,4,8,10,8] 3
foo v0.2.0.0 [1,2,4,8,10,8] 3
foo --detail v0.2.1.0 --help [1,2] --version 3 --force
```

最後の例はちょっと極端すぎましたかね？　とにかく、これまでと違う方法が必要です。一つ思い付くのはパーサーです。パーサーならば、分岐や繰り返しを実現できます。ですが、通常のパーサーではだめです。

```
foo v0.1.0.0 [1,2] 3 --version --help
foo v0.1.0.0 [1,2] 3 --help --version
```

このような並び替えも許容しなければならないからです。パーサーをコンビネータで組み合わせていくという方法では並べ替えを許容するのは骨が折れる作業です。

オプションのパーサーを提供しているライブラリがあります。 [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) です。

### optparse-applicative

optparse-applicative は "optparse" から分かる通りオプションのパーサーを提供しています。が、その後に "applicative" が付いていますね。 Applicative 型クラスを使って何かクールなことをやっていることです。

どんなクールなことなのでしょうか、 README を読んでみます。

```haskell
data Parser a

instance Functor Parser
instance Applicative Parser
instance Alternative Parser
```

この `Parser` が核となる型だそうです。これはモナドでは**ありません**。

```haskell
import Options.Applicative

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )
```

これが簡単な例として紹介されています。知らない関数がたくさん出てくるプログラムをいきなりずらっと並べられると、私は読めなくなってしまいます。なので、分解してみましょう。

```haskell
import Options.Applicative

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = undefined
```

最初の部分は普通ですね。 `Options.Applicative` だけをインポートすればよいこと、 `Sample` 型としてパースするには `Parser Sample` という型の値を作ればよいことが分かります。ここで `Sample` のような型はパースする全てのオプションと引数を含む型である必要があります。

```haskell
import Options.Applicative

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
      <$> undefined
      <*> undefined
      <*> undefined
```

`Sample` 型のそれぞれのフィールドに対応するパーサーをアプリカティブに組み立てています。この書き方はモナドなパーサーライブラリを使っているときでも出てきますが、 optparse-applicative はモナドではないので、全てをこう書く必要があります（ただし `Alternative` 型クラスによる分岐もある）。

ここまでは optparse-applicative に特有の関数は出てきませんでしたが、ここから出てきます。

```haskell
import Options.Applicative

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
      <$> strOption
          undefined
      <*> switch
          undefined
      <*> option auto
          undefined
```

三つの関数 `strOption`, `switch`, `option auto` が出てきました。これらは README の中で `Regular options` という節の中で三つ一緒に紹介されています。これらの詳しいことは後で紹介します。

```haskell
import Options.Applicative

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )
```

