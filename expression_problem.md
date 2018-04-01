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

case class Ningen {} extends Hitogata

case class Shitai {} extends Hitogata
```

### クラス (クラスベースオブジェクト指向)

```scala
class Hitogata {}

class Ningen {} extends Hitogata

class Shitai {} extends Hitogata
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

