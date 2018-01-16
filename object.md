# Haskellでオブジェクト指向

Haskellは言語拡張によって非常に型の表現力が強い。その表現力と同程度、および上回る表現力を持つ言語はScalaやCoqなど少数しか存在しない。その表現力によって、JavaやJavaScriptなどのオブジェクトをHaskell内で表現することが出来る。方法は複数あるので知っている限り紹介する。

## 自然変換

[Control.Object - Hackage](https://hackage.haskell.org/package/natural-transformation-0.4/docs/Control-Object.html)で実装されているように、ある関手fから関手IOへの自然変換は作用をもたらすことが出来るオブジェクトとして見ることが出来る。例を出して説明する。

```scala
class Person {
  def echo(s : String) : String = s
}
```

このクラスを実装したいとする。

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

```scala
class Person {
  private[this] var _age : Int = 0
  
  def age() : Unit = {
    _age = _age + 1
  }
}
```

このクラスを実装したいとする。

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

ただし、これは本来ならオブジェクトそのものが持つはずの_ageの初期値が`evalStateT`により設定されている。一般的に、この手法ではオブジェクトとその状態が別々に扱われるため、オブジェクトを別のところで使おうとしても状態は引き継がれず、状態を隠蔽することもできないという弱点がある。

## objective

[objective: Composable objects - Hackage](http://hackage.haskell.org/package/objective)で実装されているオブジェクトは、以下のように定義された型だ。

```haskell
data Object f g = Object { runObject :: forall x, f x -> g (x, Object f g) }
```

このライブラリのオブジェクトは、一言でいうと状態を持った自然変換だ。解説は良い記事があるので他の場所に譲る。

* 作者(fumieval氏)の説明([2015-Haskell-object.pdf](http://fumieval.github.io/papers/ja/2015-Haskell-objects.pdf))
* ちゅーん氏の説明([Haskellオブジェクト指向に触れてみよう～初級編～ - Creatable a => a -> IO a](http://tune.hateblo.jp/entry/2015/03/27/035648))

## まとめ

最近、[Eta](http://eta-lang.org/)が話題になっているのでHaskellでオブジェクト指向をしてみた。ゲーム制作の際に有用らしいのでいつか使ってみたい。

## 余談

この手法はEtaのJavaモナドみたいなラッパーをかぶせることが出来る。

```haskell
import Control.Object -- from objective

type Obj f g a = StateT (Object f g) g a

call :: f a -> Obj f g a
call x = StateT $ \o -> runObject o x
```
