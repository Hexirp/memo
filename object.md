# Haskellでオブジェクト指向

Haskellは言語拡張により非常に表現力が強く、これを上回るのは少数の言語しかありません。その表現力はJavaやScalaなどのオブジェクトをHaskell内で表現することが出来るほどで、方法も複数あります。何がオブジェクトなのかということも曖昧ですが、面白いと思ったので紹介します。

## コモナド

[Haskell for all: Comonads are objects](http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html)という力強いタイトルの記事で紹介されています。この記事は有名なデザインパターンのBuilder、Iterator、Commandパターンがそれぞれコモナドで表現できることを示した後に、このように議論します。

> オブジェクト指向プログラミングの概念をHaskellで実装しようとすると常に、私たちは不可避かつ自然にオブジェクトをコモナドとして模擬することになる。
> それは、コモナドを使ったプログラミングはオブジェクト指向プログラミングと同じものであることを示唆している。
> Haskellerがコモナドに苦しめられてきたのは、Haskellerがオブジェクトを使ったプログラミングを激しく拒絶しているからである。
>
> これが意味するのは、私たちはオブジェクト指向プログラミングに対して軽蔑しながら接するのではなく謙虚に接するべきであることと、コモナドでプログラミングするための適切な糖衣構文を考えだすためにオブジェクト指向プログラミングの視点を借りるべきであることである。

この後にオブジェクト指向プログラミングのような糖衣構文を提案しています。......

## 自然変換

[Control.Object - Hackage](https://hackage.haskell.org/package/natural-transformation-0.4/docs/Control-Object.html)で実装されているように、ある関手fから関手IOへの自然変換は作用をもたらすことが出来るオブジェクトとしてみることが出来ます。例えば、このような関手があるとします。

```haskell
data Person a = Echo String (String -> a)
```

そして、オブジェクトとなる自然変換を実装します。

```haskell
person :: Person a -> IO a
person (Echo s f) = return $ f s
```

こんなコードを書くことが出来ます。

```haskell
main :: IO ()
main = do
  s <- getLine
  putStrLn $ "You: " ++ s
  s' <- person $ Echo s id
  putStrLn $ "Obj: " ++ s'
```

自然変換でなくすればもっと便利です。

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
  s' <- person $ Echo s
  putStrLn $ "Obj: " ++ s'
```

これは言語内DSLを実現します。

## objective

[objective: Composable objects - Hackage](http://hackage.haskell.org/package/objective)で実装されているオブジェクトはこの記事の本命です。

```haskell
data Object f g = Object { runObject :: forall x, f x -> g (x, Object f g) }
```

自然変換でもIOを対象にする代わりにStateT s IOを対象にすれば状態を扱うことが出来ますが、そうしたものは状態と自然変換を別々に扱わないといけなくなります。このライブラリのオブジェクトは一言でいうと状態を持った自然変換で、上のように定義されています。
