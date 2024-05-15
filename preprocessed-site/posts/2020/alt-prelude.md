---
title: Alt-PreludeはHaskell 2024の夢を見るか？
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Fumiaki Kinoshita
postedBy: <a href="https://scrapbox.io/haskell-shoen/fumieval">@fumieval</a>
date: January 20, 2020
tags: Prelude
...
---

Alt-Preludeという言葉をご存知だろうか。まずは、Hackageの[このページ](http://hackage.haskell.org/packages/#cat:Prelude)をご覧いただきたい。
rio, universum, relude, 70件以上の(2020年1月現在)膨大なパッケージがずらずらと並んでいる。これらはAlt-Preludeと呼ばれるもので、その名の通りPrelude(Haskellのプログラムに暗黙のうちにインポートされるモジュール)の代わりとなるものである。
これほど多くのパッケージが作られていることは、Preludeに相応の問題があること示唆している——具体的には、

* __部分関数__: おなじみのhead, tail, init, last四兄弟に加えて、maximum、readなど、多くの関数が部分関数であり、実行時エラーになる。
* __リストだらけ__: メモリ上の表現としてリストが最適である場面は限られているにも関わらず、文字列含め何にでもリストを使っている。
* __遅延IO__: getContentsのように、リソース管理でも効率の面でも不適切な関数がある。
* __よく使うものが欠けている__: `Generic`、`ByteString`、`Text`のようなよく使う型や、`when`、`fix`などの便利な関数をインポートするために、ソースコードが何行も上乗せされる。インポートが面倒なためにその場でハックするケースも珍しくない。

Alt-Preludeは、これらの問題を解決したり、各々の目的を果たすために、以下の3つか4つを実現している。

* __危険な関数をなくす__ 部分関数のエクスポートをやめる
* __他のライブラリの型やAPIを再エクスポートする__ baseの他のモジュールや、lensのような外部パッケージを丸ごとエクスポートする。
* __独自の機能を定義する__ 独自のコンビネータ、型シノニム、データ型など
* __独自のインターフェイスを定義する__ LazyとStrictな構造を相互変換したり、文字列型の違いを吸収するための独自の型クラスを定義する。

この4本の柱により、Haskellerは便利なライブラリに簡単にアクセスできずっと幸せに暮らしましたとさ。めでたしめでた(__画面が割れる__)
