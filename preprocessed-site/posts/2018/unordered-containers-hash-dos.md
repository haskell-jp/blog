---
title: hashdos脆弱性とunordered-containers
headingBackgroundImage: ../../img/post-bg.jpg
headingDivClass: post-heading
subHeading: HashMap・HashSetの利用時は注意！
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: December 21, 2018
tags: Security
...
---

あらゆるソフトウェアに脆弱性は存在し得ます。  
Haskellは高度な型システムを駆使することで、脆弱性を根本的に回避したプログラムを作ることを可能にします<small>（脆弱性を防ぐためだけのものではないですが、興味のある人は[Safe Haskell](http://www.kotha.net/ghcguide_ja/7.6.2/safe-haskell.html)についても調べてみるといいでしょう）</small>。  
しかし、だからといって、型を設計する段階で脆弱性を回避できるよう気をつけなければいけないことには変わりませんし、GHCが生成した実行ファイル、使用するライブラリーに絶対に脆弱性がないとは言えません。  
現状、Haskellはほかの著名なプログラミング言語ほど使用されていないためか、あまり脆弱性が報告されることはありません<small>（libcなど、ほかの言語の処理系も依存しているようなライブラリーの脆弱性は別として）</small>。  
今回は、そんな中でも[unordered-containersというパッケージ](https://hackage.haskell.org/package/unordered-containers)について、[ドキュメントにも書かれている](https://github.com/tibbe/unordered-containers/blob/60ced060304840ed0bf368249ed6eb4e43d4cefc/docs/developer-guide.md#security)ため**おそらく直ることがないであろう脆弱性**と、その回避方法について紹介します。  
hashdos脆弱性自体は結構有名ですし、ドキュメントに書いてあることなので、ご存知の方には何を今更感があるかと思いますが、検索した限りこの問題について日本語で説明した記事は見当たらなかったので、ここで紹介します。

# そもそもunordered-containersって？

脆弱性の前にunordered-containersパッケージについて簡単に紹介しましょう。  
[unordered-containersパッケージ](https://hackage.haskell.org/package/unordered-containers)は、GHCに標準で付いている[containersパッケージ](https://hackage.haskell.org/package/containers)よりも高速な連想配列（[`HashMap`型](https://hackage.haskell.org/package/unordered-containers-0.2.8.0/docs/Data-HashMap-Lazy.html)）や集合（[`HashSet`型](https://hackage.haskell.org/package/unordered-containers-0.2.8.0/docs/Data-HashSet.html)）を提供してくれます。  
[StackageのLTS Haskell 10.3ではなんと970ものパッケージに依存されている](https://www.stackage.org/lts-10.3/package/unordered-containers-0.2.8.0)、超大人気汎用パッケージです。

## どうやって高速化しているの？

`HashMap`という名前が示しているとおり、キーとなる値のハッシュ値を計算・利用することで高速化しています。  
しかし、Java言語などほかの言語によくある`HashMap`とは大きく異なり、内部ではハッシュテーブルを使用していません。  
[本物のプログラマはHaskellを使う - 第35回　キーを使って値を参照するMap型：ITpro](http://itpro.nikkeibp.co.jp/article/COLUMN/20091104/340002/?rt=nocnt)でも説明しているとおり、ハッシュテーブルはミュータブルな配列を内部で使用していることから、イミュータブルなデータ構造を使用して行う関数型プログラミングとは、相性が悪いのです<small>（`ST`モナドや`IO`モナドを利用した[hashtablesパッケージ](https://hackage.haskell.org/package/hashtables)などを使えば、限られた範囲内でハッシュテーブルを使うこともできます）</small>。

ハッシュテーブルを使用しない代わりに、unordered-containersでは内部で[Hash array mapped trie](https://en.wikipedia.org/wiki/Hash_array_mapped_trie)という特殊な木を使っています。  
どのような構造かは、[HAMT ~ イミュータブルで高速なハッシュマップ ~ | κeenのHappy Hacκing Blog](http://keens.github.io/slide/HAMT/)に詳しく書かれています。  
こちらのスライドはScalaでの実装の話ですが、基本的にはunordered-containersパッケージの`HashMap`も同じはずです。

大雑把に言うと、Hash array mapped trieを使った`HashMap`では、ハッシュテーブルと同様に、キーとなる値を**ハッシュ関数で一旦固定長の整数に変換する**ことで、キーが存在しているかどうかの確認を高速化しています。そのため、containersパッケージよりも高速な処理ができるのです。  
containersパッケージの`Map`ではキーの存在を確認する際、キー全体を既存のキーと比較する必要があるため、特に長い文字列をキーとする場合は、処理が遅くなりがちだったのです。

# hashdos脆弱性とは？

hashdos脆弱性は[2011年頃RubyやPHP、Perlなど多くのプログラミング言語が影響を受けるとされた](https://blog.tokumaru.org/2011/12/webdoshashdos.html)、著名な脆弱性です。  
ここでも簡単に仕組みを説明しましょう。

前節で説明したとおり、Hash array mapped trieもハッシュテーブルも、必ずキーを一旦固定長の整数に変換します。  
文字列など、ハッシュ関数を適用されるキーとなる値は、当然固定長の整数よりも幅広い値を取り得るので、違う文字列同士でも、同じハッシュ値をとることがあります。  
この、違う値であるはずのキーが同じハッシュ値をとってしまった状態を「ハッシュ値の衝突」と呼びます。  
ハッシュ値の衝突が発生した場合、ハッシュテーブルやHash array mapped trieといったハッシュ値を利用した連想配列は、（単純な）配列やリストなど、やむを得ず逐次探索が必要なデータ構造を内部で使用しなければならなくなります。

hashdos脆弱性はこの性質を利用したDoS攻撃です。  
攻撃者は、あらかじめ対象のプログラムで使っているハッシュ関数が、「必ず同じハッシュ値」を返すキー(大抵文字列でしょう）を大量に用意して、それを対象のプログラムに入力として与えることで、簡単にDoS攻撃を仕掛けることができるのです。  
[先ほど触れた徳丸先生の記事](https://blog.tokumaru.org/2011/12/webdoshashdos.html)では、PHPのアプリケーションに対してわずか500KBのform-dataを送るだけでCPU時間を1分も消費させることができたそうですから、その威力はすさまじいものと言えるでしょう。

# なぜ直さないのか？

[unordered-containersのDeveloper Guide](https://github.com/tibbe/unordered-containers/blob/60ced060304840ed0bf368249ed6eb4e43d4cefc/docs/developer-guide.md#security)には、次のように書かれています。

> There's an uncomfortable trade-off with regards to security threats posed by e.g. denial of service attacks. Always using more secure hash function, like SipHash, would provide security by default. However, those functions would make the performance of the data structures no better than that of ordered containers, which defeats the purpose of this package.

要するに、「セキュリティー上問題はあるけど、SipHashのような安全なハッシュ関数を使ったらcontainersパッケージよりも速度が出なかった。それではこのパッケージの意味がない」ということです。  
containersパッケージよりも高速な連想配列を作るためにunordered-containersパッケージを作ったのだから、それより遅くなっては存在価値がなくなってしまうのです。  
従って、ユーザーが任意にキーを入力できるようなプログラムでは、unordered-containersではなく、containersを使え、ということです。  
このことはunordered-containersが使用している[hashableのドキュメント](https://hackage.haskell.org/package/hashable-1.2.6.1/docs/Data-Hashable.html#g:1)にも書かれています。ある意味ノーガード戦法ですね。

# 回避方法

前節で触れたとおりですが、**ユーザーが任意にキーを入力できるようなプログラム**では、unordered-containersパッケージの`HashMap`や`HashSet`ではなく、containersパッケージの`Map`や`Set`を使いましょう。  
containersパッケージにある`Map`や`Set`はハッシュ関数を一切使っていないので、ハッシュ値の衝突も起こらず、内部で逐次探索が必要なデータ構造を使ってもいません。  
なのでhashdos攻撃に遭うことはないのです。

ただし、実際のところ、[StackageのLTS Haskell 10.3で970ものパッケージに依存されている](https://www.stackage.org/lts-10.3/package/unordered-containers-0.2.8.0)unordered-containersです。  
その中にはJSONのパーサーであるaesonも含まれているので、もしかしたら現状回避するのは非常に困難なのかもしれません。😱  
次回は、この問題について試しに攻撃用のコードを書いて速度の低下をチェックして報告する話を書くかもしれません...。😰
