# Homotopy Type Theory

HoTT学習記録

## 1

自分の今のスキルはこんな感じ。

* Coq
  * 基礎となる理論であるpCICの基本的なことが分かる(ある型が付いた値はその型をβ簡約した型も付く、SetとPropとTypeの違い、Typeに階層があることなど)
  * `*_ind`系の関数がどんな法則で生成されているのか分かる。関数の停止性の証明が、再帰する際にある一つの引数が減少していけばOKという適当な感じなのが分かる
  * 基本的なタクティックの動作が分かる。まだmatchのreturnキーワードはよくわからない。標準ライブラリの仕組みもよくわからない。

目的はHoTTを理解すること。

## 2

* [The HoTT Book | Homotopy Type Theory](https://homotopytypetheory.org/book/) - 英語。どうやらプログラマではなく数学者向けらしくきつい。理解が深まった後、イメージを正確にするため読もう。
* [HoTT/Coq 覚書](https://gist.github.com/qnighy/bfb53e54d3ffcbbd0e84) - 日本語。たくさんの型が書き並べられている。重要そうなキーワードが載っている。
* [HoTT/HoTT: Homotopy Type Theory](https://github.com/HoTT/HoTT) - CoqでのHoTTの実装。よくわからない。
* [Homotopy Type Theory](https://en.wikipedia.org/wiki/Homotopy_type_theory) - Key conceptsにある対応関係は役に立った。
* [「型の理論」と証明支援システム -- COQの世界](https://www.slideshare.net/maruyama097/coq-31970579) - HoTTはメインではないが、49ページから数ページほど説明がある。
* [ホモトピー型理論入門](https://www.slideshare.net/ssuser0745d1/ho-tt-introjp20160909) - 道帰納法の具体的な直観。
* [Coqと少しの圏論が分かる人向けのhomotopy type theory(その1)](http://d.hatena.ne.jp/m-a-o/20130629%23p1) - weak ∞-groupoidというすごそうなものが出てきた。

ここまで集めた知識をまとめると、HoTTは型を空間と解釈するというのが核となるようだ。ある型Aを持つ値aは、ある空間A上の点aとなる。依存型はファイブレーションになるらしいが、ファイブレーションというものがよく分からなかったので逆にファイブレーションは依存型に対応するもの、として覚える。同値関係は道、つまりある二点をつなぐ曲線となる。ある広がりを持った空間の中の違う二点でもその間を曲線でつなぐことが出来れば同値とみなしてしまうことがホモトピーたるゆえんのようだ。すごいのは道もまた空間となること。さらにその空間の道も空間となる。これが無限に続いていく。weak ∞-groupoidであるというのはここからきているみたいだ。

Bool型みたいな普通の型は、空間としてみると点しか含まれない空間となる。その上の曲線は同じ点の間の曲線という自明なものしか存在しない。高階帰納型を使うとそうではない型を作れる。HaskellのGADTを初めて見た時のような違和感だ。

```coq
Inductive S1 : Type :=
| base : S1
| loop : base = base.
```

## 3

* [Homotopy Type Theory 入門](http://www.kurims.kyoto-u.ac.jp/~uemura/files/hott-intro-ja.pdf) - HITとかのきちんとした定義。良い文献だと思う。

Univalence Axiomの意義がちょっとわかったかもしれない。型の型を良い空間にするために必要なものだ。具体的な構成が分かってきたため、HoTTのソースコードを読むことに挑戦。まずはCategoryがどういう風に定義されているのか見る。[theories/Categories/Category/Core.v](https://github.com/HoTT/HoTT/blob/master/theories/Categories/Category/Core.v)にあった。`trunc_morphism : forall s d, IsHSet (morphism s d)`というものがある以外は普通の定義だ。`IsHSet`はHoTT覚書に書いてあって、`IsTrunc`の部分的定義らしい。

```
Class Contr_internal (A : Type) := BuildContr {
  center : A ;
  contr : (forall y : A, center = y)
}.

Inductive trunc_index : Type :=
| minus_two : trunc_index
| trunc_S : trunc_index -> trunc_index.

Fixpoint IsTrunc_internal (n : trunc_index) (A : Type) : Type :=
  match n with
    | -2 => Contr_internal A
    | n'.+1 => forall (x y : A), IsTrunc_internal n' (x = y)
  end.
```

これらがポイントとなる定義。Contrは全ての値がある一つの値に等しいということを表す。ぱっと思いつくのはUnit。`IsHProp`は`IsTrunc -1`であり、`forall x y, Contr (x = y)`。つまり、全ての値が等しく、さらにその等しいということを表す道が一つしかないということを表す。Empty、またはUnitがこれに当てはまる。円盤とかは全ての値が等しい(=全ての点が繋がっている)けど、その道が複数あるので`IsHProp`ではない。`IsHSet`は`forall x y, IsHProp (x = y)`。これは任意の二点の間の道が`IsHProp`である、つまり、ある二つの値があったとき、その間の道が一つだけあるか、ないかということ。これは普通の同値関係が入っている型だ。
