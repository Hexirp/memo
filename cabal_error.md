# cabal error

```
2019-11-19
23:23:38 +0900
hexirp
cabal で Cabal file warning ***.cabal@0:0: version with tags という警告が表示されるのですが、これってどういうことなんでしょうか？　もしかして 0.1.0.0 とかではなく 0.1 とバージョンを設定しろってことですかね？
2019-11-20
12:28:53 +0900
amderbar
@amderbar has joined the channel
2019-11-20
16:13:43 +0900
igrep
気になったのでやってみたのですが、手元では再現しませんでした。
どんなcabalファイルでどのバージョンのcabal-installを使ったら発生したんですか？
2019-11-20
16:42:21 +0900
hexirp
一例を挙げます。
環境: stack を使っています。それによって GHC 8.6.5, cabal 2.4.0.1 が使われています。 cabal-install はちょっとわかりません。
cabal ファイル: https://github.com/Hexirp/hexirp-hakyll/blob/e6b28eaf95031baf6193c8bd4bb652eefa31f70b/hexyll/hexyll.cabal
ログ (on Travis CI): https://travis-ci.org/Hexirp/hexirp-hakyll/builds/614356184
2019-11-20
16:53:09 +0900
igrep
https://github.com/haskell/cabal/issues/5022 曰くパッケージそのもののバージョンではなく依存パッケージのバージョンのことを言っている可能性もありますね... でもぱっと見おかしなところがわからない。 :confused:
2019-11-20
16:59:17 +0900
hexirp
この変更から hexyll に関する Cabal file warning ***.cabal@0:0: version with tags というエラーがなくなっているようなのですが、手掛かりになりますでしょうか
https://github.com/Hexirp/hexirp-hakyll/commit/2f857cd001109d7587ab4e82f8f15e40d5c746aa
2019-11-20
17:01:12 +0900
hexirp
もしかしてライセンスの GPL-3-or-later という指定が原因……？
2019-11-20
17:12:50 +0900
hexirp
cabal がバージョンとライセンスに同じ処理をかけていたりしているかもしれないですね
2019-11-20
19:19:56 +0900
igrep
確かに、cabalはどっかのバージョンでLICENCEで使用できる値を変えたのでそれが関係しているかも知れません
2019-11-21
02:03:50 +0900
imamura.suuri
@imamura.suuri has joined the channel
2019-11-21
09:30:38 +0900
monoid911
@monoid911 has joined the channel
2019-11-21
19:49:09 +0900
hexirp
hpack に移行したら GPL-3.0-or-later と license に書いてもエラーが出なくなりました。 cabal へ変換した後の内容は前と同じはずなのに……
2019-11-21
23:50:09 +0900
ikngtty
@ikngtty has joined the channel
2019-11-22
21:33:48 +0900
mizunashi-mana
cabal-version がファイルの先頭にない場合、旧バージョンのファイルとして前処理されます。 SPDX の式が書けるのは、新バージョンからなはずです。詳しく見てないので断言できませんが、それが関係してる気がしますね
2019-11-22
21:35:05 +0900
mizunashi-mana
hpack はその仕様が出来てから cabal-version を最初に吐き出すようになったはずですね
2019-11-22
21:58:15 +0900
mizunashi-mana
Oh… 適当を言うのはやっぱだめですね．手元で試せる環境ができたので試しましたが，まず cabal は 1.2 からファイル仕様に関してバージョン指定ができ，どのファイル仕様を使うか cabal-version で指定できるようになりました．で， cabal-version: x.y と指定するのが普通ですが，
cabal-version: >=1.10
cabal-version: >=2.0
だけ特別扱いされ，それぞれ 1.12 の形式と 2.0 の形式が選ばれます．

で， 2.2 の形式から license に書ける式が変わっており，旧来は cabal 独自の形式だったのが SPDX expression で指定するようになりました．
独自の形式の AST は， http://hackage.haskell.org/package/Cabal-3.0.0.0/docs/Distribution-License.html#t:License で，見ての通り GPL はバージョンだけしか指定できず or-later は指定できません．それが警告として表示されていて， 「GPL のライセンス指定してるんだと思うけど書く形式間違ってるんじゃない？」と言ってるんだと思います．で， hpack は多分 cabal-version: 2.4 とかで出力してるので通ってるとかではないですかね？
2019-11-22
22:26:47 +0900
hexirp
その通りでした。 hpack による出力は cabal-version: 2.2 と一番最初の個所に書いてありました。詳細な回答ありがとうございます。
2019-11-22
22:28:02 +0900
hexirp
ところで、もっと詳細に知るには https://www.haskell.org/cabal/users-guide/file-format-changelog.html と https://www.haskell.org/cabal/users-guide/developing-packages.html#package-properties を読めばOKでしょうか？
2019-11-22
23:29:49 +0900
mizunashi-mana
そうだと思います (僕が知った経緯は，たまたま cabal file の AST を最近いじろうと思ったことがあってっていうだけなので，本当にそのドキュメントを読むだけで良いかはちょっと自信ないですが)
2019-11-22
23:31:16 +0900
mizunashi-mana
(現状， cabal の仕様は複雑になっていく一方なんですが，エラーメッセージはかなり不親切で cabal user guide もあまり網羅性がないので，置いてけぼり食ってるユーザかなりいそうな気がするんですよね…)
2019-11-23
12:12:05 +0900
hexirp
エラーメッセージが不親切なのは、現に私はこの問題を最初はパッケージのバージョンに関する問題だと勘違いしてしまったわけで、かなり問題そうですね。
```
