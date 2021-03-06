# MinecraftのMOD開発

[akariki](https://github.com/Hexirp/akariki)の開発記録。Minecraft 1.12とWindows 10でやっていく。

## 環境構築

「環境構築」は自分のパソコンでMOD開発ができるようにすること。「やりたいことが出てきたのでチュートリアルでやっている方法から少し変えよう」と思うと、Gradleのスキル、及びMinecraft Forgeの仕組みに対しての理解を要する。一番最初にやることなのにここが一番難しいかもしれない。自分のプロジェクトでは[MinecraftForge導入手順](http://minecraftjp.info/modding/index.php/MinecraftForge導入手順)を参考にして好きなフォルダでMOD開発ができるような環境構築をした。

まず、Minecraft Forge MDKをダウンロードして`C:\minecraft\forge`とかの好きな場所に解凍をする。`C:\minecraft\mod`とかのMOD開発を始めたいフォルダに必要最低限のもの(`src`、`gradle`、`gradlew.bat`、`gradlew`、あとなんか)をコピーする。その後は[MinecraftForge導入手順](http://minecraftjp.info/modding/index.php/MinecraftForge導入手順)と同じことをすればよい。

## IntelliJ IDEAの使用

Javaは何かをするときに書かなければいけない量が多いのでIDEが欲しい。IntelliJ IDEAはGradleでビルドするプロジェクトを簡単にIDEAの中のプロジェクトとして使える。さらに[MinecraftForge導入手順](http://minecraftjp.info/modding/index.php/MinecraftForge導入手順)に書いてある`runClient`と`runServer`によるデバッグもIDEAの中ですることが出来る。

まず、環境構築を終えた後に出来たプロジェクトを`Import Project from Gradle`という機能でIDEAの中のプロジェクトとして取り込む。その後、IDEAを終了して`genIntellijRuns`を`setupDecompWorkspace`と同じような感じでやる。またIDEAを開けばデバッグができるようになっている。

## Event

普通のJavaのプログラムはmainメソッドがあってそこから実行を開始する。MODもプログラムであり、mainメソッドで実行するものは「アイテムを追加する」「ブロックを追加する」などの処理である。しかし、MODにははmainメソッドが不要である。その代わりに`@Mod`アノテーションを使ってmainメソッドがあるようにみなすクラスを指定できる。複数`@Mod`アノテーションがある場合、一つのプロジェクトの中に複数のMODが入っているという風になる。さらに、`@Mod`アノテーションを使ったクラスの中で`@EventHandler`アノテーションを使うことでmainメソッドのようにみなすメソッドを指定できる。そのmainメソッドのようにみなすメソッドはある特定の引数を持っていなければならない(本物のmainメソッドが`String arg[]`という引数を取らなければならないようなもの)。その引数によってそのメソッドでどのような処理をしてもいいのかが違い、さらに引数が違えばmainとみなされるメソッドが複数あっても良く、それぞれのメソッドで別々の役割を持たせる。

## Typo

一週間悩んだエラーの原因が[これ](https://github.com/Hexirp/akariki/commit/6ef9013d1108c5864e104f526042a797e945d556)だった。

## Gradle

Forgeが生成するサンプルプロジェクトをそのまま使っていても、自分でプロジェクトを作っても、Forgeが新しくなった時にMinecraftのバージョン、Forgeのバージョン、MCPのMappingのバージョンを自分で上げなければならなくなる。その時に[最新のサンプルプロジェクトの設定](https://github.com/MinecraftForge/MinecraftForge/blob/07c4da8f3611266a80f432a81dc1a6f0f3f649ac/mdk/build.gradle)を参考にすることがよい。残念ながら生成される前のソースコードの時点では`@MAPPINGS@`という風になっているが、これはGradleによりビルド時に置き換え処理が行われるものだからソースコードを`MAPPINGS`とかで検索していって辿ればどんなバージョンが代入されるのか分かる。[こんな感じに。](https://github.com/MinecraftForge/MinecraftForge/blob/083daeb5ed8e01518132c441f68eca88e2d59800/build.gradle#L192)

## ModelRegistryEvent

Modelの登録がModelRegistryEventにより発火されるようになったみたいだ。PreInitializationEventの時にModelを登録しようとすると「時々」モデルがおかしくなる。これを知るまでに[苦労](https://github.com/Hexirp/akariki/issues/1)した。

## プリズマリンのフェンス

1.13でフェンスが水を通すようになるとの情報がある。確かに自然だが、気持ち悪い。不思議な力で水を通さないフェンスとしてプリズマリンのフェンスを追加したい。
