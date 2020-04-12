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

そんなHaskeller達の悩みの種を軽減しようと、GHC 8.0以降、[`Strict`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#strict-by-default-pattern-bindings)と[`StrictData`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#strict-by-default-pattern-bindings)という拡張が作られました。  
これらの拡張を大雑把に言うと、

- `StrictData`: 値コンストラクターにおいて、引数の値が弱頭正規形（Weak Head Normal Form。以降慣習に従い「WHNF」と呼びます）まで評価されるようになる
- `Strict`: 値コンストラクターを含めたあらゆる関数やローカル変数の定義において、パターンマッチで代入した変数の値がWHNFまで評価されるようになる

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

なお、使用したGHCのバージョンは8.6.5で、OSはWindows 10 ver. 1909です。

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

# Case hoge: ポイントフリースタイルかどうかで変わる！

hoge

# Case hoge: 内側のパターンはやっぱりダメ

hoge

## [strict-types](https://github.com/pepeiborra/strict-types)が使えるかも

hoge

# Case hoge: `foldl`と`foldr`

hoge

# Case hoge: `undefined`を受け取るメソッド

hoge

