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
