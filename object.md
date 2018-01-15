# Haskellでオブジェクト指向

Haskellは、言語拡張によって非常に型の表現力が強い。その表現力と同程度、および上回る表現力を持つ言語はScalaやCoqなど少数しか存在しない。その表現力によって、JavaやJavaScriptなどのオブジェクトをHaskell内で表現することが出来る。方法は複数あるので知っている限り紹介する。

## コモナド

[Haskell for all: Comonads are objects](http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html)という力強い(?)タイトルの記事で紹介されている。この記事は、有名なデザインパターンのBuilder、Iterator、Commandパターンがそれぞれコモナドで表現できることを示した後に、このように議論する。

> Every time we try to implement object-oriented programming in Haskell we gravitate, inexorably, to modeling objects as comonads.
> This suggests that object-oriented programming and comonadic programming are one and the same.
> Haskell programmers have struggled with comonads because we so militantly rejected object-oriented programming.
>
> This means we should approach object oriented programming with humility instead of disdain and borrow object-oriented insight to devise a suitable syntactic sugar for programming with comonads.

拙訳するとこんな感じだと思われるが、英語は上手ではないので間違いがあるかもしれない。

> オブジェクト指向プログラミングの概念をHaskellで実装しようとすると常に、私たちは不可避かつ自然にオブジェクトをコモナドとして模擬することになる。
> それは、コモナドを使ったプログラミングはオブジェクト指向プログラミングと同じものであることを示唆している。
> Haskellerがコモナドに苦しめられてきたのは、Haskellerがオブジェクトを使ったプログラミングを激しく拒絶しているからである。
>
> これが意味するのは、私たちはオブジェクト指向プログラミングに対して軽蔑しながら接するのではなく謙虚に接するべきであることと、コモナドでプログラミングするための適切な糖衣構文を考えだすためにオブジェクト指向プログラミングの視点を借りるべきであることである。

この後にオブジェクト指向プログラミングのようにコモナドを扱う糖衣構文を提案している。

## 自然変換

[Control.Object - Hackage](https://hackage.haskell.org/package/natural-transformation-0.4/docs/Control-Object.html)で実装されているように、ある関手fから関手IOへの自然変換は作用をもたらすことが出来るオブジェクトとして見ることが出来る。例を出して説明する。

```haskell
-- 関手になる。実装は省略。
data Person a = Echo String (String -> a)

-- 自然変換の一つ。
person :: Person a -> IO a
person (Echo s f) = return $ f s

-- このようなコードに利用することが出来る。
main :: IO ()
main = do
  s <- getLine
  putStrLn $ "You: " ++ s
  s' <- person $ Echo s id
  putStrLn $ "Obj: " ++ s'
```

実はfは関手ではない方が便利になる。

```haskell
{-# LANGUAGE GADTs #-}

data Person a where
  Echo :: String -> Echo String

person :: Person a -> IO a
person (Echo s) = return s

main :: IO ()
main = do
  s <- getLine
  putStrLn $ "You: " ++ s
  s' <- person $ Echo s -- idがいらなくなる
  putStrLn $ "Obj: " ++ s'
```

これは言語内DSLを実現する。状態を扱うためにはこのようにすればよい。

```haskell
{-# LANGUAGE GADTs #-}

data Person a where
  Age :: Echo ()

person :: Person a -> StateT Int IO a
person Age = modify (1 +)

main :: IO ()
main = flip evalStateT 0 $ do
  s0 <- get
  lift $ putStrLn $ "Now: " ++ show s0
  person $ Age
  s1 <- get
  lift $ putStrLn $ "Now: " ++ show s1
```

ただし、これはオブジェクトたる自然変換と状態が別々に扱われるので、オブジェクトを別のところで使おうとしても状態は引き継がれないという弱点がある。

## objective

[objective: Composable objects - Hackage](http://hackage.haskell.org/package/objective)で実装されているオブジェクトは、以下のように定義された型だ。

```haskell
data Object f g = Object { runObject :: forall x, f x -> g (x, Object f g) }
```

このライブラリのオブジェクトは、一言でいうと状態を持った自然変換だ。解説は良い記事があるので他の場所に譲る。

* 作者(fumieval氏)の説明([2015-Haskell-object.pdf](http://fumieval.github.io/papers/ja/2015-Haskell-objects.pdf))
* ちゅーん氏の説明([Haskellオブジェクト指向に触れてみよう～初級編～ - Creatable a => a -> IO a](http://tune.hateblo.jp/entry/2015/03/27/035648))

## 余談

コモナドと自然変換の二つの手法は互いに関連しあっている気がしている。

```haskell
import Control.Comonad
import Control.Object -- objective

newtype Obj f g a = Obj { unObj :: (a, Object f g) }

instance Functor (Obj f g) where
  fmap f (Obj (x, o)) = Obj (f, o)

instance Comonad (Obj f g) where
  extract (Obj (x, _)) = x
  
  extend f (Obj (x, o)) = Obj (f (Obj (x, o)), o)
```

このコモナドは次のような自然変換を持つ。圏論的にオブジェクトを扱うのならばこの式が重要かもしれない。

```haskell
-- Obj f g ∘ f ~> g ∘ Obj f g
call :: Functor g => Obj f g (f a) -> g (Obj f g a)
call (Obj (x, o)) = fmap Obj (runObject o x)
```

これを一般化してこんなクラスを作ったりしてもいいかもしれない。

```haskell
{-# LANGUAGE TypeFamilies　#-}

class Object w where
  type Source w :: * -> *
  type Target w :: * -> *
  
  call :: w (Source w a) -> Target w (w a)
```

ここまで妄想に付き合ってくれてありがとう。
