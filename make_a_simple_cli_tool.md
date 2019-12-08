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

どんなクールなことなのでしょうか、 README を読んでみます。ここからは、 Hackage にある README を元にした話なので、細かい情報を知りたい場合は元々のものを読むことをお勧めします。

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

`Sample` 型のそれぞれのフィールドに対応するパーサーをアプリカティブに組み立てています。この書き方はモナドなパーサーライブラリを使っているときでも出てきますね。

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

三つの関数 `strOption`, `switch`, `option auto` が出てきました。これらは README の中で `Builders` という節の中で紹介されています。これらの詳しいことは後で紹介します。

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

`strOption`, `switch`, `option auto` は共通する形式の設定を受け取っています。その設定はモノイドになっています。ここの詳細も後程紹介します。

これでパーサーの作り方は大体理解できました。次は、パーサーの実行方法です。

```haskell
main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: Sample -> IO ()
greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()
```

今回もまた知らない関数がたくさん出てきて目が滑ってしまいました。

```haskell
main :: IO ()
main = greet =<< execParser opts
  where
    opts = undefined

greet :: Sample -> IO ()
greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()
```

`opts` の部分はいったん置いておきましょう。すると、パーサーを実行した結果を bind で実際の処理を行う関数 `greet` で渡すという普通の処理になりますね。

```haskell
main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: Sample -> IO ()
greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()
```

この `info` だとか `(<**>)` だとかよく分からない関数が出てくる箇所は、 `--help` をオプションとして渡されたらヘルプを出力できるようにする部分だと説明されています。とにかく、 `sample` をこう包んで、 `progDesc` や `header` などのヘルプに表示するためのメッセージを適切に置き換えれば良さそうです。

ちなみに調べてみたら `a <**> f` は `flip ($) <$> a <*> f` と等しいそうです。

```
    hello - a test for optparse-applicative

    Usage: hello --hello TARGET [-q|--quiet] [--enthusiasm INT]
      Print a greeting for TARGET

    Available options:
      --hello TARGET           Target for the greeting
      -q,--quiet               Whether to be quiet
      --enthusiasm INT         How enthusiastically to greet (default: 1)
      -h,--help                Show this help text
```

ヘルプはこんな感じに表示されると書かれています。ここで、これまでの例で `help` とか `metavar` とか `progDesc` とか `header` とかで設定した文字列がどこに表示されているのか見てみてください。

### optparse-applicative の builder

オプションや引数などを表す `strOption` や `switch` などの関数は builder と README で総称されています。

```haskell
outputFile :: Parser String
outputFile = strOption
             ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> value "out.txt"
            <> help "Write output to FILE" )
```

`strOption` は オプションの引数として与えられた文字列をそのまま得ることができます。

```haskell
lineCount :: Parser Int
lineCount = option auto
            ( long "lines"
           <> short 'n'
           <> metavar "K"
           <> help "Output the last K lines" )
```

`option auto` は `Read` 型クラスを利用してオプションの引数を文字列から別の型に変えて得ることができます。

```haskell
keeping :: Parser Bool
keeping = switch
          ( long "keep-tmp-files"
         <> help "Retain all intermediate temporary files" )
```

`switch` はフラグを定義できます。例えば、この場合では `--keep-tmp-files` オプションが渡されたとき `True` になり渡されていないとき `False` となります。

```haskell
usingFile :: Parser String
usingFile = argument str (metavar "FILE")
```

`argument` は引数を表します。 optparse-applicative という名前ですが、引数なども取り扱えるというわけですね。

設定にも様々なものがあります。 `long`, `short` はコマンドの名前です。長い名前と短い名前の二つがあり、それぞれ `String` 型と `Char` 型を受け取ります。 `value` はデフォルトの値です。 `metavar` と `help` はヘルプに関するものです。

他にも様々な builder があり、サブコマンドに対応するものもあります。さらに、 optparse-applicative の `Parser` 型は `Alternative` 型クラスのインスタンスも持ちます。これによって、可変長引数や両立しないオプションやその他の様々なことが出来ます。

これで optparse-applicative の説明を終わりにします。

### パーサーの実装

optparse-applicative の節で書いたことを使えば簡単です。

```haskell
module Main where

  import Prelude
  import Options.Applicative

  -- オプションや引数やフラグなどを全て含む型
  data Config = Config
    { version :: String
    , sequence :: [Integer]
    , number :: Integer
    , withVersionInfo :: Bool
    , withDetail :: Bool
    , forcing :: Bool
    }

  -- コマンドの引数をパースする
  optparse :: Parser Config
  optparse = Config
    <$> argument auto (metavar "VERSION")
    <*> argument auto (metavar "SEQ")
    <*> argument auto (metavar "NUM")
    <*> switch
      ( mempty
      <> long "version"
      <> short 'v'
      <> help "Print the command's version"
      )
    <*> switch
      ( mempty
      <> long "detail"
      <> short 'd'
      <> help "Print details"
      )
    <*> switch
      ( mempty
      <> long "force"
      <> short 'f'
      <> help "Force to use deprecated versions"
      )

  -- パーサーを実行する
  main :: IO ()
  main = do
      conf <- execParser opts
      fooApp conf
    where
      opts = info (flip ($) <$> optparse <*> helper)
        ( mempty
        <> progDesc "Print a result of foo"
        <> header "foo - a command for foo"
        )

  fooApp :: Config -> IO ()
  fooApp = undefined
```

引数を受け取るようなオプションがなかったのでこんなに簡単になったのかもしれません。ここで、調べる必要があったのは `argument auto` の部分だけでした。その部分は `option` と同じようにオプションの引数をパースするときの戦略のようなものを受け取る個所でした。 `auto` なら Read 型クラスを使って変換し、 `str` なら文字列のままにする、となっていました。

## 最後に

皆さんも optparse-applicative を使いましょう。

ちなみに、 optparse-applicative はさっきも言った通り Applicative がベースなのでかなりの自由度があります。なので、 [オレ的 Haskell で CLI を作る方法 2018](https://matsubara0507.github.io/posts/2018-05-10-make-cli-with-haskell-in-2018.html) のように他のライブラリと簡単に組み合わせたりできます。 Applicative 様様ですね。

依存関係も軽い（再帰的に依存しているライブラリの量が少ない）ので、気軽にプロジェクトに入れることもできますよ。

## 参考文献

* [optparse-applicative/README.md at master · pcapriotti/optparse-applicative](https://github.com/pcapriotti/optparse-applicative/blob/b861da1e6b021d6abd75ff7e9a4277939aa7a541/README.md)
