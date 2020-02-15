---
title: Strict拡張を使用する際の注意点
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
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
- `Strict`: 値コンストラクターを含めたあらゆる関数やローカル変数の定義において、パターンマッチで束縛した変数の値がWHNFまで評価されるようになる

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

# Case hoge: `where`句だろうとなんだろうと評価

hoge

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

