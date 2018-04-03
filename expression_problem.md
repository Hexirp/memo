# Expression Problem

Expression Problemについて調べたことなど。

## 導入

あるまとまりがあるとしよう。そうだな、人型(`Hitogata`)とでもしよう。当然、人間(`Ningen`)はそれに含まれるだろう。死体(`Shitai`)もだ。
これを静的に表現しようと思う。様々な表現方法がある。

### 代数的データ型

```haskell
data Hitogata = Ningen | Shitai
```

```scala
sealed trait Hitogata {}

case class Ningen extends Hitogata {}

case class Shitai extends Hitogata {}
```

### クラス (クラスベースオブジェクト指向)

```scala
trait Hitogata {}

class Ningen extends Hitogata {}

class Shitai extends Hitogata {}
```

### 型クラス

```haskell
class Hitogata t where {}

data Ningen = MkNingen
data Shitai = MkShitai

instance Hitogata Ningen where {}
instance Hitogata Shitai where {}
```

```scala
trait Hitogata[A] {}

class Ningen {}
class Shitai {}

implicit val hitogataNingen = new Hitogata[Ningen] {}
implicit val hitogataShitai = new Hitogata[Shitai] {}
```

### オブジェクト (objective)

```haskell
data Hitogata a

ningen :: Object Hitogata f
ningen = Object $ \x -> case x of {}

shitai :: Object Hitogata f
shitai = Object $ \x -> case x of {}
```

## 関数

この人型というまとまり全体を扱う関数が欲しい。たとえば、それが生き物であるなら真を返し、そうでないなら偽を返す関数だ。ただし、元の実装に手を加えてはいけない！以下のコードを下に追加する形で、正常に動いてほしい。

### 代数的データ型

```haskell
isLiving :: Hitogata -> Bool
isLiving x = case x of
  Ningen -> True
  Shitai -> False
```

```scala
object Function {
  def isLiving(x : Hitogata) : Boolean = x.match {
    case Ningen => true
    case Shitai => false
  }
}
```

### クラス (クラスベースオブジェクト指向)

```scala
// TODO
```

### 型クラス

```haskell
-- TODO
```

```scala
// TODO
```

### オブジェクト (objective)

```haskell
-- TODO
```
