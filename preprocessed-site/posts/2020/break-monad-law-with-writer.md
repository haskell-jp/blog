---
title: Writer Monadで気軽にMonad則を破る
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: YAMAMOTO Yuji
postedBy: <a href="http://the.igreque.info/">YAMAMOTO Yuji(@igrep)</a>
date: December 25, 2020
tags: Monad
...
---

🎅この記事は、[Haskell Advent Calendar 2020](https://qiita.com/advent-calendar/2020/haskell) 25日目の記事です。Happy Christmas!!🎅

今回は先日<small>（といっても元の質問の投稿からもう何ヶ月も経ってしまいましたが...）</small>StackOverflowに上がったこちら👇の質問に対する回答の、続きっぽい話を書こうと思います。長いし、本来の質問の回答からスコープが大きく外れてしまうので記事にしました。

[haskell - モナド則を崩してしまう例が知りたい - スタック・オーバーフロー](https://ja.stackoverflow.com/questions/70079/%E3%83%A2%E3%83%8A%E3%83%89%E5%89%87%E3%82%92%E5%B4%A9%E3%81%97%E3%81%A6%E3%81%97%E3%81%BE%E3%81%86%E4%BE%8B%E3%81%8C%E7%9F%A5%E3%82%8A%E3%81%9F%E3%81%84)

簡単にMonad則を破る例を紹介することで、Monad則のみならず`do`記法やMonadそのものの性質について、よりはっきりとした理解が得られることを期待します。

# サンプルコードについて

本記事のサンプルコードは、Haskellの構文に準拠していないものを除いて、すべて[readme-test](https://github.com/igrep/readme-test)というツールの[2020年12月13日時点の開発版](https://github.com/igrep/readme-test/tree/f6ce7a6f5ce5f5f8031cd5dfedc8c6e47c13b1f3)でテストしました。こちらのツールはまだ開発中で、今後も仕様が大きく変わる可能性がありますが、この記事のサンプルコードをテストするのに必要な機能は十分にそろっています。このreadme-test自体についてはいつか改めて共有します。

また、テストの際に用いた環境は以下の通りです:

- Windows 10 Pro 20H2
- GHC 8.10.1
- [Stackage nightly-2020-08-15](https://www.stackage.org/nightly-2020-08-15)

# `Monad`と`Monoid`の切っても切り離せない関係

「[モナドは単なる自己関手の圏におけるモノイド対象だよ。何か問題でも？](http://www.aoky.net/articles/james_iry/brief-incomplete-and-mostly-wrong.htm)」というフレーズ（原文「[A monad is a monoid in the category of endofunctors, what's the problem?](http://james-iry.blogspot.com/2009/05/brief-incomplete-and-mostly-wrong.html)」が示すとおり、モナドとモノイド、Haskellの識別子で言うところの`Monad`と`Monoid`には密接な関係があります。ぶっちゃけ、このフレーズの正確な意味を私は理解していないのですが、少なくとも`Monad`と`Monoid`には重要な共通点があることは知っています。それは、どちらも**単位元と結合則**がある、ということです！

具体的に`Monad`と`Monoid`の単位元・結合則を見てみましょう:

<!-- ReadmeTest: Ignore -->

`Monoid`の単位元: 単位元である`mempty`は、どんな値`x`に`<>`で足しても結果が変わらない！

```haskell
x <> mempty = x
mempty <> x = x
```

`Monad`の単位元: `return`は`>>=`の前に使っても後ろに使っても、`m`や`k a`の結果を変えない！

```haskell
return a >>= (\a -> k a) = k a
m >>= (\a -> return a) = m
```

`Monoid`の結合則: `x <> y <> z`の結果は、`y <> z`を先に計算しようと`x <> y`を先に計算しようと変わらない！

```haskell
x <> (y <> z) = (x <> y) <> z
```

`Monad`の結合則: `m >>= \x -> k x >>= h` の結果は、`\x -> k x >>= h`を先に計算しようと、`m >>= (\x -> k x)`を先に計算しようと変わらない！

```haskell
m >>= (\x -> k x >>= h) = (m >>= (\x -> k x)) >>= h
```

※`Monad`の単位元・結合則の式についてはわかりやすさのために[引用元](http://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Monad.html#t:Monad)から少し形を変えています。

Haskellにおける`Monad`・`Monoid`とは、値がそれぞれの単位元・結合則をを満たす型です[^applicative-functor]。それ以上でも、それ以下でもありません。

それぞれの単位元・結合則を表す式は、一見して異なるものに見えるかも知れませんが、表す性質自体はよく似ています。なので、式を読んでもよく分からないという方は、上記に書いた日本語の説明をざっと眺めて覚えておいてください。特に、結合則における**「～を先に計算しようと、～を先に計算しようと変わらない！」**の部分がこの後とても重要になります。

[^applicative-functor]: 一応、`Monad`についてはそのスーパークラスである`Applicative`の則、`Functor`の則がありますが、Monad則を満たしていればそれらは自動的に満たせるので、ここでは省略します。

{#monoid-examples}
## `Monoid`の例

ここまで読んで、`Monad`はなんか聞いたことがあるけど`Monoid`は初めて聞くよ、という方向けに補足すると、`Monoid`とは例えば次のような型の値（と、それに対する処理）です。

`Sum`型: 数値<small>（Num型クラスのインスタンス）</small>に対する、足し算を表すMonoidのインスタンス

<!-- ReadmeTest: AppendAsIs -->

```haskell
-- これから紹介する処理に必要なモジュールのimport
import Data.Monoid
```

<!-- ReadmeTest: CompareAfterPrompt ByExpression -->

```haskell
-- Sum aに対する <> は + と同等なので、
> getSum (Sum 1 <> Sum 2 <> mempty)
-- は、
1 + 2 + 0
-- と同じ。
```

`mempty`が各`Monoid`のインスタンスにおける単位元を返す、という点に注意してください。上記のとおり足し算の場合は`0`です。

`Product`型: 数値<small>（Num型クラスのインスタンス）</small>に対する、かけ算を表すMonoidのインスタンス

```haskell
-- Product aに対する <> は * と同等なので、
> getProduct (Product 1 <> Product 2 <> mempty)
-- は、
1 * 2 * 1
-- と同じ。
```

リスト型: リスト型の値に対する、結合 `(++)`

```haskell
-- [a] に対する <> は ++ と同等なので、
> [1, 2] <> [3] <> mempty
-- は、
[1, 2] ++ [3] ++ []
-- と同じ
```

`All`型: `Bool`型の値に対する論理積`&&`を表すMonoidのインスタンス

```haskell
> getAll (All True <> All False)
-- は、
True && False
-- と同じ
```

<!-- ReadmeTest: ValidateAsExpression -->

```haskell
-- これが何を返すかは、想像してみてください！
getAll mempty
```

`Any`型: `Bool`型の値に対する論理和`||`を表すMonoidのインスタンス

<!-- ReadmeTest: CompareAfterPrompt ByExpression -->

```haskell
> getAny (Any True <> Any False)
-- は、
True || False
-- と同じ
```

<!-- ReadmeTest: ValidateAsExpression -->

```haskell
-- これも何を返すかは、想像してみてください！
getAny mempty
```

このように、`Monoid`は他のプログラミング言語でもおなじみの、多くの二項演算を表しています。これらのインスタンスはすべて、先ほど紹介した「単位元」や「結合則」のルールを守っているので、気になった方はぜひチェックしてみてください[^float]。

[^float]: 残念ながら実際のところ、`Float`型・`Double`型などの浮動小数点数に対する`Sum`や`Product`は結合則を満たさない場合があります。これは他の多くのプログラミング言語にもある、浮動小数点数の悩ましい問題です。詳しくは「情報落ち」で検索してみてください。

# `Monoid`と`Writer`の切っても切り離せない関係

実はそんな`Monad`と`Monoid`の固い絆を象徴するような`Monad`が、この世にはあります。そう、`Writer`です！`Writer`は`Monoid`の単位元・結合則をそのまま活かすことによって`Monad`の単位元・結合則を満たした`Monad`であり、`Writer`がどうやって`Monad`則を満たしているのか知れば、`Monad`則がどうやって成立するものなのかが、すっきりクリアになることでしょう。

手始めに`Writer`の定義と、`Writer`が`Monad`の各メソッドをどのように実装しているか見てみましょう。[「モナドのすべて」における`Writer`の紹介ページ](https://www.sampou.org/haskell/a-a-monads/html/writermonad.html)から、少しリファクタリングしつつ引用します[^transformers]。

[^transformers]: ここでの定義は、実際に使われている[transformersパッケージ](http://hackage.haskell.org/package/transformers)の`Writer`の定義とは大きく異なっているのでご注意ください。実際の`Writer`はパフォーマンス上の都合やMonad Transformerとの兼ね合いで、幾分工夫された定義となっています。

<!-- ReadmeTest: AppendAsIs -->

```haskell
-- Writer型の定義
newtype Writer w a = Writer { runWriter :: (a, w) }
```

`newtype Writer w a = Writer { runWriter :: (a, w) }`という定義のとおり、`Writer`の実態はただのタプルです。ただのタプルがどうやって`Monad`になるのでしょう？その答えがこちら👇:

<!--
```haskell
instance Functor (Writer w) where
  fmap f (Writer (x, w)) = Writer (f x, w)

instance Monoid w => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  Writer (f, w1) <*> Writer (x, w2) = Writer (f x, w1 <> w2)
```
-->

```haskell
-- WriterのMonad型クラスの実装
-- 実際のところFunctor, Applicativeのインスタンス定義も必要だけどここでは省略
instance Monoid w1 => Monad (Writer w1) where
  return a = Writer (a, mempty)
  Writer (a, w1) >>= f =
    let (b, w2) = runWriter $ f a
     in Writer (b, w1 <> w2)
```

`return`の定義は比較的シンプルですね。単位元である`mempty`を受け取った値`a`と一緒にタプルに入れて返すだけです。`Monad`の単位元である`return`では、`Monoid`単位元である`mempty`を使うのです。

一方、`>>=`はどう読めばいいでしょう？`let ... in ...`の結果にあたる`Writer (b, w1 <> w2)`に注目してください。

まず、`b`は`>>=`の右辺である`f`が返した結果です。`Writer`はタプルを単純に`newtype`でラップしただけのものであり、ラップしたタプルの一つ目の要素は、ここで`f`が返した値の型と一致していなければなりません。`Writer`において`>>=`の型は`Writer w a -> (a -> Writer w b) -> Writer w b`で、右辺にあたる`f`は`(a -> Writer w b)`という型なので、`>>=`全体の戻り値`Writer w b`と`f`の戻り値が一致している必要があることがわかりますよね？

さらに重要なのが`w1 <> w2`です。ここであの`Monoid`の演算子`<>`が出てきました！`Writer`は`>>=`の中で`<>`を使う`Monad`なんですね！一体何と何を`<>`しているのでしょう？まず、`<>`の左辺である`w1`は、左辺にあたる`Writer`がタプルに保持していた`Monoid`型クラスのインスタンスの値です。そして右辺の`w2`は、`>>=`の右辺に渡した関数`f`が`b`と一緒に返した`w2`です。

以上のことをまとめると、`Writer`の`>>=`は、

1. 左辺の`(a, w1)`における`a`を`f`に渡して、
1. `f`が返した`(b, w2)`における`b`を、
1. `w1`と`w2`と一緒に`<>`でくっつけつつ返す、

という処理を行っています。`Writer`は、「`b`を返すついでに`w1`と`w2`を`<>`でくっつける」と覚えてください。

`Writer`は`Monad`の単位元`return`で`Monoid`の単位元`mempty`を使って、`Monad`の結合則を満たす`>>=`でこれまた`Monoid`の結合則を満たす`<>`を使っているのです。やっぱり`Writer`は`Monoid`あっての`Monad`と言えますね。

## `do`と`<>`

さて、この「`b`を返すついでに`w1`と`w2`を`<>`でくっつける」という`Writer`の振る舞いが象徴するように、大抵の`Monad`インスタンスにおける`>>=`は、**何かしら値を返すついでに、何らかの処理を行うよう実装**されています。この「ついでに」行われる処理は`Monad`のインスタンスを`do`記法の中で扱うと、ますます静かに身を隠すようになります。

こちらも`Writer`を例に説明しましょう。まず、例示用に`Writer`を作るアクションを適当に定義します。

```haskell
addLogging :: Int -> Int -> Writer [String] Int
addLogging x y =
  Writer (x + y, ["Adding " ++ show x ++ " to " ++ show y ++ "."])

multLogging :: Int -> Int -> Writer [String] Int
multLogging x y =
  Writer (x * y, ["Multiplying " ++ show x ++ " with " ++ show y ++ "."])
```

`addLogging`と`multLogging`はそれぞれ、引数として受け取った整数を足し算したりかけ算したりしつつ、「足したよ」「かけたよ」という内容の文字列を一緒に返します。`Writer [String] Int`における`[String]`にログとして書き込んでいるようなイメージで捉えてください。

これらを`do`の中で使ってみると、より`addLogging`や`multLogging`が「足し算やかけ算をするついでに、ログとして書き込んでいる」っぽいイメージが伝わるでしょう:

⚠️申し訳なくも`do`記法自体の解説、つまり`>>=`がどのように`do`記法に対応するかはここには書きません。お近くのHaskell入門書をご覧ください。

```haskell
testDo :: Writer [String] Int
testDo = do
  result1 <- addLogging 3 4
  result2 <- multLogging 5 2
  addLogging result1 result2
```

👆では、`3 + 4`した結果`result1`と、`5 * 2`した結果`result2`を足す処理を行っています。それに加えて、「足したよ」「かけたよ」というログを表す文字列のリスト`[String]`も一緒に返しています。`do`記法が`>>=`に変換されるのに従い、`Writer`の`>>=`が内部で`<>`を使い、`addLogging 3 4`・`multLogging 5 2`・`addLogging result1 result2`が返した文字列のリスト`[String]`を結合することによって、あたかも`addLogging`や`multLogging`が「値を返しつつ、ログとして書き込む」かのような処理を実現できるのが`Writer`における`do`記法の特徴です。

<!-- ReadmeTest: CompareAfterPrompt ByExpression -->

能書きはここまでにして、実際にどのような結果になるか見てみましょう:

```haskell
> runWriter testDo
(17,["Adding 3 to 4.","Multiplying 5 with 2.","Adding 7 to 10."])
```

はい、`3 + 4`と`5 * 2`の結果を足し算した結果`17`と、`addLogging 3 4`・`multLogging 5 2`・`addLogging result1 result2`が一緒に返していた文字列のリスト`[String]`が、書いた順番どおりに結合されて返ってきました。`Writer`は`do`記法の中に書いた`Writer`の値`(a, w)`のうち、`Monoid`のインスタンスである`w`を`<>`で都度結合させているということが伝わったでしょうか？

## `Writer` Monadの結合則と`Monoid`の結合則

ここまでで、`Writer` Monadがどのように`<>`を使っているのか、それによって`>>=`や`do`記法がどのように振る舞っているのか、具体例を示して説明いたしました。ここからは、`Writer`が`Monoid`の`<>`の結合則をどう利用することで、`Monad`としての`>>=`の結合則を満たしているのかを示しましょう。長いので「めんどい！」という方は[こちらをクリックしてスキップ](#associative-law-qed)してください。

そのために、`Monad`の結合則における`>>=`を、`Writer`の`>>=`として展開してみます。

<!-- ReadmeTest: Ignore -->

(0) `Monad`の結合則:

```haskell
m >>= (\x -> k x >>= h) = (m >>= (\x -> k x)) >>= h
```

(1) `m`は`>>=`の左辺なので`Writer (a, w1)`に置き換える:

※ここからは、比較しやすくするために等式`=`の左辺と右辺を別々の行に書きます。

```haskell
let Writer (a, w1) = m in Writer (a, w1) >>= (\x -> k x >>= h)
  =
let Writer (a, w1) = m in Writer (a, w1) >>= (\x -> k x) >>= h
```

(2) 一つ目の`>>=`を`Writer`における`>>=`の定義で置き換える:

```haskell
let Writer (a, w1) = m
    (b, w2) = runWriter $ (\x -> k x >>= h) a
 in Writer (b, w1 <> w2)
  =
let Writer (a, w1) = m in Writer (a, w1) >>= (\x -> k x) >>= h
```

(3) 等式`=`の右辺における一つ目の`>>=`も同様に変換する:

```haskell
let Writer (a, w1) = m
    (b, w2) = runWriter $ (\x -> k x >>= h) a
 in Writer (b, w1 <> w2)
  =
let Writer (a, w1) = m
  (b, w2) = runWriter $ (\x -> k x) a
 in Writer (b, w1 <> w2) >>= h
```

(3) 無名関数である`(\x -> k x >>= h)`と`(\x -> k x)`に、`a`を適用する:

```haskell
let Writer (a, w1) = m
    (b, w2) = runWriter $ k a >>= h
 in Writer (b, w1 <> w2)
  =
let Writer (a, w1) = m
  (b, w2) = runWriter $ k a
 in Writer (b, w1 <> w2) >>= h
```

(4) 二つ目の`>>=`を`Writer`における`>>=`の定義で置き換える:

```haskell
let Writer (a, w1) = m
    (b, w2) = runWriter $
      let Writer (c, w3) = k a
          (d, w4) = h c
       in Writer (d, w3 <> w4)
 in Writer (b, w1 <> w2)
  =
let Writer (a, w1) = m
    (b, w2) = runWriter $ k a
 in Writer (b, w1 <> w2)) >>= h
```

(5) 等式`=`の左辺における二つ目の`>>=`も同様に変換する:

```haskell
let Writer (a, w1) = m
    (b, w2) = runWriter $
      let Writer (c, w3) = k a
          (d, w4) = h c
       in Writer (d, w3 <> w4)
 in Writer (b, w1 <> w2)
  =
let Writer (a, w1) = m
    (b, w2) = runWriter $ k a
 in let Writer (c, w3) = Writer (b, w1 <> w2)
        (d, w4) = runWriter $ h c
     in Writer (d, w3 <> w4)
```

(6) `Writer`と`runWriter`は、`Writer`と`(a, w)`を切り替えるだけで実質何もしていないので削除する:

```haskell
let (a, w1) = m
    (b, w2) =
      let (c, w3) = k a
          (d, w4) = h c
       in (d, w3 <> w4)
 in (b, w1 <> w2)
  =
let (a, w1) = m
    (b, w2) = k a
 in let (c, w3) = (b, w1 <> w2)
        (d, w4) = h c
     in (d, w3 <> w4)
```

(6.5) (6)の等式をよく見ると、`=`の左辺においては`(b, w2)`と`(d, w3 <> w4)`が、`=`の右辺においては`(c, w3)`と`(b, w1 <> w2)`が等しい。

```haskell
let (a, w1) = m
    (b, w2) = --           ここの(b, w2)は、
      let (c, w3) = k a
          (d, w4) = h c
       in (d, w3 <> w4) -- ここの(d, w3 <> w4)を代入したもの！
 in (b, w1 <> w2)
  =
let (a, w1) = m
    (b, w2) = k a
 in let (c, w3) = (b, w1 <> w2) -- ここで代入している！
        (d, w4) = h c
     in (d, w3 <> w4)
```

(7) (6.5)から、`=`の左辺では`b = d`で`w2 = w3 <> w4`、`=`の右辺では`c = d`で`w3 = w1 <> w2`であることがわかる。なのでそれぞれ置き換える:

```haskell
let (a, w1) = m
    (c, w3) = k a
    (d, w4) = h c
 in (d, w1 <> (w3 <> w4))
  =
let (a, w1) = m
    (b, w2) = k a
    (d, w4) = h b
 in (d, (w1 <> w2) <> w4)
```

<div id="associative-law-qed"></div>

(8) `a`～`d`・`w1`～`w4`の変数名を、登場した順番に振り直す:

```haskell
let (a, w1) = m
    (b, w2) = k a
    (c, w3) = h b
 in (c, w1 <> (w2 <> w3))
  =
let (a, w1) = m
    (b, w2) = k a
    (c, w3) = h b
 in (c, (w1 <> w2) <> w3)
```

等式`=`の左辺と右辺がそっくりな式になりましたね！

ここで、`Monoid`の結合則を思い出してみましょう:

```haskell
x <> (y <> z) = (x <> y) <> z
```

そう、`x <> y <> z`などと書いて3つの`Monoid`型クラスのインスタンスの値を`<>`でくっつけるときは、カッコで囲って`(y <> z)`を先に計算しようと、`(x <> y)`を先に計算しようと、結果が変わらない、というものでした！

それを踏まえて、(8)の等式`=`の両辺をよく見比べてみてください。異なっているのは`w1 <> (w2 <> w3)`と`(w1 <> w2) <> w3)`の箇所だけですね！つまり、`Writer` Monadにおける`>>=`の結合則は、`w1 <> (w2 <> w3)`と`(w1 <> w2) <> w3)`が等しいから、すなわち`Monoid`における`<>`の結合則が成り立つからこそ成立するのです。これがまさしく「`Monoid`と`Writer`の切っても切り離せない関係」なのです！

# 関係を壊してみる

それではいよいよ、「`Monoid`と`Writer`の切っても切り離せない関係」を利用して、Monad則を破ってみましょう💣

## `<>`と`Monoid`の結合則

前述のとおり、`Writer`における`>>=`が結合則を満たすのは、`Writer`がラップしている`Monoid`な値の`<>`が結合則を満たしてこそ、なのでした。これは言い換えれば、その、ラップしている`Monoid`な値の`<>`が結合則を破れば、自然に`Writer`の`>>=`も結合則を破るはずです。この方法は、結合則を満たさない`>>=`っぽい処理をゼロから探すより遥かに簡単です。`>>=`のような`m a -> (a -> m b) -> m b`というややこしい型の関数よりも、`<>`のような`a -> a -> a`という型の関数の方がずっと身近ですしね！

`Monoid`の`<>`のような`a -> a -> a`という型の関数で、結合則を満たさない関数 --- といえば、引き算`-`や割り算`/`を思い浮かべる方が多いのではないでしょうか。と、いうわけで[`Monoid`の例](#monoid-examples)で紹介した`Sum`や`Product`のように、数値に対する引き算を表す`newtype`、`Difference`を定義してみましょう:

<!-- ReadmeTest: AppendAsIs -->

```haskell
newtype Difference a = Difference { getDifference :: a }
```

それから、`Difference`を<small>（実際には間違いですが）</small>`Monoid`のインスタンスにします。最近のGHCでは、`Monoid`のインスタンスを定義する前に`Semigroup`のインスタンスにする必要があるのでご注意ください。説明しやすさのために敢えてこれまで触れてきませんでしたが、これまで何度も使った`<>`は実際のところ`Monoid`の関数ではなく`Semigroup`の関数なんですね。`Monoid`は「`<>`で（結合則を備えた）二項演算ができるだけでなく、`mempty`という単位元もある」という性質の型クラスなので、「単に『`<>`で（結合則を備えた）二項演算ができる』だけの型クラスも欲しい！」というニーズから、`Monoid`の`<>`は`Semigroup`の関数となり、`Monoid`は`Semigroup`のサブクラスという関係に変わったのでした。

何はともあれ、`Difference`を`Semigroup`のインスタンスにしましょう:

```haskell
instance Num a => Semigroup (Difference a) where
  Difference a <> Difference b = Difference (a - b)
```

はい、単に両辺を`-`で引き算するだけですね。

今度こそ`Difference`を`Monoid`のインスタンスにします。本記事では`mempty`を直接使うことはないので何でもいいはずですが、とりあえず`Sum`と同様に`0`ということにしておきます:

```haskell
instance Num a => Monoid (Difference a) where
  mempty = Difference 0
```

😈これで`<>`が結合則を満たさないおかしな`Monoid`のインスタンス、`Difference`ができました！早速試して結合則を破っていることを確認してみましょう:

<!-- ReadmeTest: CompareAfterPrompt ByExpression -->

```haskell
-- こちらは 1 - (2 - 3) と同じ
> getDifference $ Difference 1 <> (Difference 2 <> Difference 3)
2

-- こちらは (1 - 2) - 3 と同じなので...
> getDifference $ (Difference 1 <> Difference 2) <> Difference 3
-4 -- <- 当然 1 - (2 - 3) とは異なる結果に！
```

バッチリ破れてますね！このように`<>`における結合則は、引き算などおなじみの演算で、簡単に破ることができます💪

## `>>=`と`Monad`の結合則

`<>`における結合則を破ることができたと言うことは、`Writer`の`>>=`による結合則は、もはや破れたも同然です。先ほど定義した`Difference`型を使えば、`>>=`は途端に結合則を満たさなくなるでしょう。

例を示す前に、`Writer`を使う際しばしば用いられる、ユーティリティー関数を定義しておきます。実践で`Writer`を使いたくなったときにも大変便利なので、是非覚えておいてください:

<!-- ReadmeTest: AppendAsIs -->

```haskell
tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)
```

この`tell`関数は、受け取った`Monoid`な値をそのまま「ログとして書き込む」関数です。結果として返す値はただのユニット`()`なので、気にする必要がありません。`tell`のみを使って`Writer`を組み立てれば、「ログとして書き込む」値のみに集中することができます。これから紹介する例でもやはり関心があるのは「ログとして書き込む」値だけなので、ここで`tell`を定義しました。

それでは`tell`を使って、`Writer`の`>>=`における結合則も破ってみましょう:

<!-- ReadmeTest: CompareAfterPrompt ByExpression -->

```haskell
-- こちらは Difference 1 <> (Difference 2 <> Difference 3) と同じ
> getDifference . snd . runWriter $ tell (Difference 1) >>= (\_ -> tell (Difference 2) >>= (\_ -> tell (Difference 3)))
2

-- こちらは (Difference 1 <> Difference 2) <> Difference 3 と同じなので...
> getDifference . snd . runWriter $ (tell (Difference 1) >>= (\_ -> tell (Difference 2))) >>= (\_ -> tell (Difference 3))
-4 -- <- 当然 1 - (2 - 3) とは異なる結果に！
```

予想どおり一つ目の`Writer`と二つ目の`Writer`とで異なる結果となりました。`1 - (2 - 3)`と`(1 - 2) - 3`を`Writer`を使って遠回しに言い換えているだけなので、当然と言えば当然です。

しかし`tell (Difference 1) >>= (\_ -> tell (Difference 2) >>= \_ -> tell (Difference 3))`などの`Writer`型の式が`Monad`の結合則`m >>= (\x -> k x >>= h) = (m >>= (\x -> k x)) >>= h`にどう対応するのか、ちょっと分かりづらいですかね<small>（式も長いし）</small>？一つずつ注釈を加えます:

```haskell
-- こちらは m >>= (\x -> k x >>= h) = (m >>= (\x -> k x)) >>= h の前半、
--   m >>= (\x -> k x >>= h) に相当する
>  tell (Difference 1) >>= (\_ -> tell (Difference 2) >>= (\_ -> tell (Difference 3)))
-- ^^^^^^^^^^^^^^^^^^^       ^    ^^^^^^^^^^^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
--          m                x             k                             h
--

-- こちらは m >>= (\x -> k x >>= h) = (m >>= (\x -> k x)) >>= h の後半、
--   (m >>= (\x -> k x)) >>= h に相当する
>  (tell (Difference 1) >>= (\_ -> tell (Difference 2))) >>= (\_ -> tell (Difference 3))
--  ^^^^^^^^^^^^^^^^^^^       ^    ^^^^^^^^^^^^^^^^^^^       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
--           m                x             k                             h
```

ラムダ式の引数`x`は実際には使われていない点に注意してください。これでも`const`を使って`\x -> const (tell (Difference 2)) x`と書き換えれば、`const (tell (Difference 2))`が`k`に厳密に対応するので、上記の二組の式は`>>=`の結合則を破るペアだと言えます。

## `do`記法と`Monad`の結合則

前の節では、`Monoid`の結合則を守っていない値をラップしている`Writer`を作ることで、`>>=`の結合則を破る例を簡単に作り出せることを紹介しました。ここでは本記事の最後として、`>>=`の結合則を破った結果、`do`記法がいかに直感に反する挙動となるか紹介して、`>>=`の結合則を守ることが私たちにどのようなメリットをもたらすのか解説します。

例として、先ほど`>>=`の結合則を破るのに使った`1 - 2 - 3`を再利用しましょう。`Difference`をラップした`Writer`で`1 - 2 - 3`を計算させると、次のような式になります:

<!-- ReadmeTest: ValidateAsExpression -->

```haskell
tell (Difference 1) >>= (\_ -> tell (Difference 2)) >>= (\_ -> tell (Difference 3))
```

これを`do`記法に変換すると、次のようになります:

```haskell
do
  tell (Difference 1)
  tell (Difference 2)
  tell (Difference 3)
```

`do`記法における各行の間に`>>=`が隠れたことで、すっきりしましたね！

この状態から、`do`記法を使って`1 - (2 - 3)`と`(1 - 2) - 3`を表す`Writer`の式にするには、次のように書き換えます:

<!-- ReadmeTest: AppendAsIs -->

```haskell
-- こちらが 1 - (2 - 3) を表す
do_1minus'2minus3' =
  do
    tell (Difference 1)
    do
      tell (Difference 2)
      tell (Difference 3)

-- こちらが (1 - 2) - 3 を表す
do_'1minus2'minus3 =
  do
    do
      tell (Difference 1)
      tell (Difference 2)
    tell (Difference 3)
```

コメントに書いたとおり、`do_1minus'2minus3'`が`1 - (2 - 3)`、`do_'1minus2'minus3`が`(1 - 2) - 3`と同等な`Writer`です。Haskellはシングルクォートを変数の名前に含めることができるので、シングルクォートでカッコを表すことにしました<small>（まさかこんなところで役に立つとはね！）</small>。

上記の二つの式では、カッコ`()`で囲う代わりにもう一つの`do`記法に収めることで、`do`記法における各行を実行する順番をいじっています。

本当にこれで`1 - (2 - 3)`や`(1 - 2) - 3`と同等な式になっているのでしょうか？試しに`runWriter`して結果を確かめてみましょう:

<!-- ReadmeTest: CompareAfterPrompt ByExpression -->

```haskell
-- こちらが 1 - (2 - 3) を表す
> getDifference . snd $ runWriter do_1minus'2minus3'
2

-- こちらが (1 - 2) - 3 を表す
> getDifference . snd $ runWriter do_'1minus2'minus3
-4
```

バッチリ👌想定どおり、`do_1minus'2minus3'`が`1 - (2 - 3) = 2`を計算し、`do_'1minus2'minus3`が`(1 - 2) - 3 = -4`を計算していますね！

さてこれまでで、`Writer` Monadは`Monoid`の結合則を利用することで`>>=`の結合則を満たしていることを示し、ラップしている`Monoid`な値が結合則を満たしていなければ、必然的に`Writer`も結合則を破ってしまうことを、`>>=`や`do`記法を使って具体的に示しました。それでは今挙げた、`do`記法で結合則を破った例は、一体何を示唆しているのでしょうか？普通にHaskellでコードを書いていて、前述のような書き換え、すなわち、

<!-- ReadmeTest: ValidateAsExpression -->

```haskell
do
  tell (Difference 1)
  do
    tell (Difference 2)
    tell (Difference 3)
```

から、

```haskell
do
  do
    tell (Difference 1)
    tell (Difference 2)
  tell (Difference 3)
```

への書き換え<small>（あるいはその逆）</small>は、一見するとそんな機会ないように思えます。しかしこれが、`do`記法をカッコ代わりに使うという変な方法ではなく、次のように変数に代入することで切り出していた場合、いかがでしょうか？

<!-- ReadmeTest: AppendAsIs -->

```haskell
someSingleAction = tell (Difference 1)

someSequence = do
  tell (Difference 2)
  tell (Difference 3)

someCompositeAction = do
  someSingleAction
  someSequence
```

上記👆のような三つの`Writer`の値を、下記👇の三つの値にリファクタリングする場合です。

```haskell
refactoredSequence = do
  tell (Difference 1)
  tell (Difference 2)

splitOutSingleAction = tell (Difference 3)

refactoredCompositeAction = do
  refactoredSequence
  splitOutSingleAction
```

あるいは、たった3行しかありませんし、一つの値に統合する方がいいかも知れません:

```haskell
flattenedAction = do
  tell (Difference 1)
  tell (Difference 2)
  tell (Difference 3)
```

これらの書き換えは、いずれも`do`記法が内部で使っている`>>=`の結合則を前提とすれば、可能であってしかるべきです。`do`記法は、適当に`Monad`のインスタンスの値（「アクション」などとも呼ばれます）を上から下まで列挙すれば、自動で`>>=`を使ってつなげてくれる、というものです。なので、適当に並べたアクションがどういう形に結合されるのか気にする必要があるのでは、安心して使えません。一方、上記の3組の式は、`Writer Difference`、すなわち引き算を表す「偽`Monoid`」をラップしているが故に、`>>=`の結合則を満たしておりません。結果、`do`記法に変えたときに並べたアクションをどこで切り出すかで、結果が変わってしまいます。これでは安心して列挙できません！

<!-- ReadmeTest: CompareAfterPrompt ByExpression -->

<!--
```haskell
> getDifference . snd $ runWriter someCompositeAction
2
> getDifference . snd $ runWriter refactoredCompositeAction
-4
> getDifference . snd $ runWriter flattenedAction
2
```
-->

# まとめ

以上です。これまでで、Monad則のうち結合則がなぜ重要なのか、結合則を実際に破ってみることを通じて説明しました。`Monad`と同様に結合則を持った`Monoid`は、`Monad`以上にインスタンスを見つけるのが簡単で、なおかつ、例えば引き算のように「二項演算だけど結合則を満たしていない」処理を見つけるのが簡単です。本記事では`Monoid`のそうした性質と、`Monoid`の性質でもってMonad則を満たしている`Writer` Monadに注目することで、簡単にMonad則を破る例を提示することができました。それから、`Monad`の結合則を実際に破った例を使って、`Monad`の結合則が`do`記法を自然に書けるようにするために必要であることを示しました。これらの実例から主張したいことを一般化すると、次のとおりです:

- `do`記法の各行の間で、値を返すついでに何かを行うのが`Monad`のインスタンス
- `do`記法の各行の間で、値を返すついでに行っている処理が結合則を満たす型が、Monad則を満たすと言える
- `Monad`則を守らない型を`do`記法で使うと、`do`記法の結合を気にして書かなければならなくなる

それでは、2021年も🎁Happy Haskell Hacking with Monad🎁!
