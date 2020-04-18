---
title: Strict拡張を使用する際の注意点
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: YAMAMOTO Yuji
postedBy: <a href="http://the.igreque.info/">YAMAMOTO Yuji(@igrep)</a>
date: January 26, 2020
tags:
...
---


Haskellは他の多くのプログラミング言語と異なった特徴を持っており、しばしばそれらが議論を呼ぶことがあります。  
その中でも特によく俎上に上がるのが、遅延評価です。  
遅延評価は、適切に扱えば不要な計算を行わず、計算資源を節約してくれるステキな仕組みですが、一歩使い方を間違うと「サンク」という「これから実行する<small>（かも知れない）</small>計算」を表すオブジェクトが大量の作られてしまい、却ってメモリー消費量が増えてしまう、などといった問題を抱えています。  
この現象は「スペースリーク」と呼ばれ、かつて[専門のAdvent Calendar](https://qiita.com/advent-calendar/2015/haskell-space-leaks)が作られたことがあるほど、Haskeller達の関心を集めてきました。

そんなHaskeller達の悩みの種を軽減しようと、GHC 8.0以降、[`Strict`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#strict-by-default-pattern-bindings)と[`StrictData`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#strict-by-default-pattern-bindings)という拡張が搭載されました。  
これらの拡張を大雑把に言うと、

- `StrictData`: 値コンストラクターにおいて、引数の値が弱頭正規形（Weak Head Normal Form。以降慣習に従い「WHNF」と呼びます）まで評価されるようになる
- `Strict`: `StrictData`の効果に加え、あらゆる関数やローカル変数の定義において、パターンマッチで代入した変数の値がWHNFまで評価されるようになる

というものです。

このうち、`StrictData`は比較的リスクが少なく大変有用（もはや標準であって欲しいぐらい）という声をよく聞きますが[^strictdata-sample]、`Strict`については様々な問題点があることが知られています。  
今回はその各種問題点をまとめて共有することで、思い切って`Strict`を有効にするときに参考になる情報を提供したいと思います！

[^strictdata-sample]: 例えばfumievalさんによる[この記事](http://fumieval.hatenablog.com/entry/2015/12/10/200630)。

# 前提知識とその参考資料

以下の知識について、概要を理解しているものとして進めます。  
参考になりそうな日本語のページも付記したので、ご覧ください。

- Haskellの遅延評価について
    - [実装して理解する遅延評価の仕組み 〜 thunkを絵に描いて理解しよう・JavaScriptでHaskellを実装！？ - プログラムモグモグ](https://itchyny.hatenablog.com/entry/20130209/1360417348)が分かりやすいでしょう
- Haskellの正格評価について
    - [正格性のすべて (翻訳)](https://haskell.e-bigmoon.com/posts/2018/06-25-all-about-strictness)
- `Strict`と`StrictData`について
    - [Strict Haskell - あどけない話](https://kazu-yamamoto.hatenablog.jp/entry/20151117/1447726679)
- その他、[Haskellスペースリーク Advent Calendar 2015 - Qiita](https://qiita.com/advent-calendar/2015/haskell-space-leaks)の記事にも有用なものがたくさんあります。

# サンプルコードの試し方

これから紹介するコードは、すべて[このブログのリポジトリーの、`examples`ディレクトリー](https://github.com/haskell-jp/blog/tree/master/examples/2020/strict-gotchas)に置いておきました。  
下記のコマンドを実行すれば実際に試すことができます。

```
git clone https://github.com/haskell-jp/blog.git
cd blog/examples/2020/strict-gotchas
stack exec runghc -- <これから紹介するコードのファイル>.hs
```

実際に試すときは`-XStrict`というオプションを`runghc`に付けた場合と付けなかった場合両方で実行して、違いを確かめてみてください。

なお、使用したGHCのバージョンは8.8.3で、OSはWindows 10 ver. 1909です。

# Case 1: `where`句だろうとなんだろうと評価

最初のケースは、遅延評価で当たり前に享受していたメリットが、`Strict`を有効にしている状態では得られなくなってしまう、というものです。  
[pfxfncさんのStrict拡張でハマったお話](https://qiita.com/pxfnc/items/a26bda6d11402daba675)という記事でも紹介されてはいますが、まとめ記事なのでここでも改めて取り上げます。

```haskell
main :: IO ()
main = print $ div10 0

div10 :: Int -> Int
div10 n
  | n == 0    = 0
  | otherwise = result
 where
  result = 10 `div` n
```

ご覧のとおり、本当にほとんどpfxfncさんの記事のサンプルそのままで恐縮ですが、このプログラム、👇のように`Strict`拡張を有効にして実行するとエラーが起こります。

```bash
> stack exec -- runghc --ghc-arg=-XStrict where.hs
where.hs: divide by zero
```

一方、`Strict`拡張を有効にしなかった場合、エラーは起こりません。

```bash
> stack exec -- runghc where.hs
0
```

なぜこんなことが起こるのでしょう？

これは、`Strict`拡張がパターンマッチで代入したあらゆる変数の値をWHNFまで評価するようになった結果、`where`句で代入した変数まで必ずWHNFまで評価してしまうために発生したエラーです。  
すなわち、`where`における、

```haskell
  result = 10 `div` n
```

までもが、

```haskell
  !result = 10 `div` n
```

とBangパターンを付けた代入であるかのように解釈されたのです[^bangpatterns]。

[^bangpatterns]: `BangPatterns`言語拡張を有効にした上で上記のように書き換えてみると、`Strict`拡張の有無に関わらずエラーが発生します。試してみましょう。

こうなると、`result`を使用しないケース、すなわち`n == 0    = 0`の場合であっても`result`に <small>（WHNFまで評価した）</small>値を代入するのに必要な計算は実行され、結果<code>10 `div` 0</code>が計算されようとして`divide by zero`が発生するのです。

`where`句は関数定義の後ろの方に書くという性格上、見落としがちかも知れません。注意しましょう。

# Case 2: ポイントフリースタイルかどうかで変わる！

続いて、Haskellに慣れた方なら誰もが一度は試したくなる、ポイントフリースタイルに関する落とし穴です。  
まずは次の二つの関数をご覧ください。

```haskell
dontReferArgs :: a -> b -> a
dontReferArgs = const

referArgs :: a -> b -> a
referArgs x _ = x
```

この関数、どちらもやっていることは`const`と変わりません。  
`dontReferArgs`は`const`をそのまま使うことでポイントフリースタイルにしていますが、`referArgs`は自前で引数に言及することで`const`と同等の定義となっています。  
ポイントフリースタイルに変えると言うことは原則として元の関数の挙動を変えないワケですから、`dontReferArgs`と`referArgs`の意味は変わらないはず、ですよね[^opt]？  

[^opt]: 実際のところ今回紹介するケース以外にも、ポイントフリースタイルにするかしないかで実行効率などが変わる場合があります。例えば、[Evaluation of function calls in Haskell](https://treszkai.github.io/2019/07/13/haskell-eval)をご覧ください。

ところがこれらの関数を`Strict`拡張を有効にした上で定義すると、なんと挙動が異なってしまいます！

使用例:

```haskell
main :: IO ()
main = do
  print $ dontReferArgs "dontReferArgs" (undefined :: Int)
  print $ referArgs "referArgs" (undefined :: Int)
```

実行結果（Strict拡張を有効にしなかった場合）:

```bash
> stack exec runghc const.hs
"dontReferArgs"
"referArgs"
```

実行結果（Strict拡張を有効にした場合）:

```bash
> stack exec -- runghc --ghc-arg=-XStrict const.hs
"dontReferArgs"
const.hs: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries\base\GHC\Err.hs:80:14 in base:GHC.Err
  undefined, called at const.hs:10:34 in main:Main
```

はい、`where`句のケースと同様、`Strict`拡張を有効にした場合、例外が発生してしまいました❗️  
`Strict`拡張を有効にした結果、意図せず例外を発生させる値<small>（今回の場合`undefined`）</small>が評価されてしまったのです。

例外を発生させた関数はそう、ポイントフリースタイルでない、`referArgs`関数の方です！  
なぜ`referArgs`でのみ例外が発生してしまったのかというと、`referArgs`が`Strict`拡張を有効にしたモジュールで、引数に言及しているからです。  
`Strict`拡張を有効にした結果「あらゆる関数やローカル変数の定義において、パターンマッチで代入した変数の値」が評価されるようになるとおり、`referArgs`の引数`x`・`_`も必ず評価されるようになり、このような例外が発生したのです。  
たとえ使用しない変数`_`でも関係ありません！

そのため、原因の本質は引数に言及<small>（してパターンマッチ）</small>しているか否かであり、`Prelude`の`const`を使用しているか否かではありません。  
こちら👇のように引数に言及した上で`const`を使っても、結果は同じなのです。

```haskell
referArgsByConst :: a -> b -> a
referArgsByConst x y = const x y
```

```haskell
print $ referArgsByConst "referArgsByConst" (undefined :: Int)
```

一方、`dontReferArgs`については、引数に言及せず、`Prelude`にある`const`をそのまま使っています。  
`Strict`拡張ではあくまでも「パターンマッチした変数」のみをWHNFまで評価するようになるものであり、あらゆる関数が正格に呼び出されるわけではありません。  
なので通常の`Prelude`における`const`と同様、`dontReferArgs`も第2引数は評価しないため、`undefined`を渡しても例外は起こらなかったのです。

このことは、「`Strict`拡張を有効にしているモジュールの中でも、`Strict`を有効にしていないモジュールから`import`した関数は、引数を正格に評価しない」という忘れてはならないポイントも示しています。  
例えば`const`よりももっと頻繁に使われるであろう、言及する引数を一つ削除する演算子の代表である、関数合成`.`を使ったケースを考えてみてください。

ポイントフリースタイルに慣れた方なら、関数適用`$`を次👇のように使って定義した`f`を見ると、

```haskell
f xs = map (+ 3) $ filter (> 2) xs

-- あるいは、`$`を使わないでこのように書いた場合も:
f xs = map (+ 3) (filter (> 2) xs)
```

こちら👇のように書き換えたくなってうずうずするでしょう。

```haskell
f = map (+ 3) . filter (> 2)
```

しかし、`Strict`を有効にしたモジュールでこのような書き換えを行うと、`f`の挙動が変わってしまいます。  
引数`.`を使って書き換える前は、引数`xs`に言及していたところ`.`を使って引数`xs`に言及しなくなったからです。  
こうした書き換えによって、**`Strict`拡張を有効にしていても意図せず遅延評価してしまう**というリスクがあるので、リファクタリングの際はくれぐれも気をつけてください[^list]。  
ざっくりまとめると、`Strict`拡張を有効にしているモジュールでは、「引数や変数を宣言することすなわちWHNFまで評価すること」、あるいは「引数や変数を宣言しなければ、評価されない」と意識しましょう。

[^list]: もっとも、この場合引数はリストでしょうから、WHNFまでのみ正格評価するメリットは少なそうですが。

ちなみに、`referArgs`における`_`のように「`Strict`拡張を有効にした場合さえ、使用していない引数が評価されてしまうのは困る！」という場合は、引数名の前にチルダ`~`を付けてください。

```haskell
referArgs :: a -> b -> a
referArgs x ~_ = x
```

# Case hoge: 内側のパターンはやっぱりダメ

hoge

# Case hoge: `foldl`と`foldr`

hoge

# Case hoge: `undefined`を受け取るメソッド

hoge

