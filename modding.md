# MinecraftのMOD開発

[akariki](https://github.com/Hexirp/akariki)の開発記録。Minecraft 1.12とWindows 10でやっていく。

## 環境構築

「環境構築」は自分のパソコンでMOD開発ができるようにすること。「やりたいことが出てきたのでチュートリアルでやっている方法から少し変えよう」と思うと、Gradleのスキル、及びMinecraft Forgeの仕組みに対しての理解を要する。一番最初にやることなのにここが一番難しいと感じた。自分のプロジェクトでは[MinecraftForge導入手順](http://minecraftjp.info/modding/index.php/MinecraftForge導入手順)を参考にして好きなフォルダでMOD開発ができるような環境構築をした。

まず、Minecraft Forge MDKをダウンロードして`C:\minecraft\forge`とかの好きな場所に解凍をする。`C:\minecraft\mod`とかのMOD開発を始めたいフォルダに必要最低限のもの(`src`、`gradle`、`gradlew.bat`、`gradlew`、あとなんか)をコピーする。その後は[MinecraftForge導入手順](http://minecraftjp.info/modding/index.php/MinecraftForge導入手順)と同じことをすればよい。

## IntelliJ IDEAの使用

Javaは何かをするときに書かなければいけない量が多いのでIDEが欲しい。IntelliJ IDEAはGradleでビルドするプロジェクトを簡単にIDEAの中のプロジェクトとして使える。さらに[MinecraftForge導入手順](http://minecraftjp.info/modding/index.php/MinecraftForge導入手順)に書いてある`runClient`と`runServer`によるデバッグもIDEAの中ですることが出来る。

まず、環境構築を終えた後に出来たプロジェクトを`Import Project from Gradle`という機能でIDEAの中のプロジェクトとして取り込む。その後、IDEAを終了して`genIntellijRuns`を`setupDecompWorkspace`と同じような感じでやる。これで終わり。
