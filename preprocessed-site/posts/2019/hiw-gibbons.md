---
title: HIW 2019で発表された、Gibbonコンパイラーについて
subHeading: ～HIW 2019参加レポート その3～
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: September 18, 2019
tags: Haskell Implementors' Workshop
...
---

[前回](/posts/2019/hiw-ghc-future.html)から引き続き、[Haskell Implementors' Workshop 2019](https://icfp19.sigplan.org/home/hiw-2019#About)への参加レポートとして、私の印象に残った発表を紹介します。  
今回は、[Gibbon](http://iu-parfunc.github.io/gibbon/)という、GHC以外のHaskell<small>（の、サブセット）</small>の処理系についての発表です。

# The Gibbon Compiler: Accelerating a small subset of Haskell

発表者: Ryan R. Newton *Indiana University*, Michael Vollmer *Indiana University, USA*, Chaitanya Koparkar *Indiana University*

Gibbonは最適化の手法を研究するために作られたコンパイラーです。  
具体的には、我々<small>（特にHaskeller）</small>がよく使う、木構造全体に対する処理の最適化です。

こうした木構造のデータは、通常ポインターを使ってメモリー内にバラバラに格納されますが、Gibbonによる最適化を行うと、実際にプログラムがどのような順番で木を処理しているのか解析して、（元のデータ構造を配列に変換した上で）その順番に並べられた配列として処理するコードに変換する、という大胆な変換を行います。
図にするとこんなイメージでしょうか？

![変換前の木構造。各ノードがそれぞれ（レベル順で）A, B, ... , Gという値を持っている](/img/2019/hiw-gibbons/tree-and-array1.svg)

👆のような木構造があったとして、

![行きがけ順（A, B, D, E, C, F, Gの順）でアクセスする](/img/2019/hiw-gibbons/tree-and-array2.svg)

👆における、赤い線の順番<small>（行きがけ順）</small>にアクセスする関数があったとします。  
適当にHaskellの再帰関数として書くと、👇こういうコードです。

```haskell
data Tree = Node Char (Maybe Tree) (Maybe Tree) deriving Show

tree :: Tree
tree =
  Node 'A'
    ( Just
      ( Node 'B'
        (Just (Node 'D' Nothing Nothing))
        (Just (Node 'E' Nothing Nothing))
      )
    )
    ( Just
      ( Node 'C'
        (Just (Node 'F' Nothing Nothing))
        (Just (Node 'G' Nothing Nothing))
      )
    )

preOrder :: (Char -> IO ()) -> Tree -> IO ()
preOrder access (Node char mLeft mRight) = do
  access char

  case mLeft of
    Just left -> preOrder access left
    Nothing -> return ()

  case mRight of
    Just right -> preOrder access right
    Nothing -> return ()
```

Gibbonはこの関数と、それが処理する木構造を解析して、

![変換された配列。A, B, D, E, C, F, Gの順に要素が並んだただの配列](/img/2019/hiw-gibbons/tree-and-array3.svg)

👆のような、ただの配列（とそれに対する関数）にまとめて変換してしまう、というのです！

現代のコンピューターは、このような配列の要素にまとめてアクセス処理する方が、ポインターをたどって各要素を処理するより、たいてい遙かに速いです。  
Gibbonはこの特性を活かすべく、我々Haskellerが好んで使うような、ポインターだらけの木構造を可能な限り配列に変換することで、要素をまとめて処理する（traverseする）演算の最適化を図るコンパイラーです。

ちなみに、元の木に対するノードの追加に相当する処理は、新しいノードに対するポインターを書き込む処理に変換するそうです。  
なので何度も追加を繰り返すと、あまり恩恵が受けられなくなってしまいそうです。

なかなか興味深いアイディアですが、個人的に聞きそびれた疑問が2つあります。  
一つは、そもそも木構造を定義するような状況というのは、いろいろな順番でアクセスしたいし、新しい要素の追加も繰り返し行いたいケースではないでしょうか？  
例えば[unordered-containers](http://hackage.haskell.org/package/unordered-containers)にある`HashMap`型は探索木を使った頻繁に使われるデータ構造ですが、`HashMap`を使う場合に行う処理の多くは、ランダムアクセスや要素の追加・削除でしょう。

なので、Gibbonが最適化したい「木構造」というのは、どちらかというと探索木のような木ではなく、構文木のような、要素をまとめて処理することを前提とした木のことなのかもしれません。  
確かに人間が書く言語の構文木程度であれば、すべてメモリー上で処理できる程度のサイズに収まる<small>（という想定でなければコンパイラー作りがものすごく難しくなる）</small>でしょうし、構文木の処理を高速化できれば、遅い遅いと言われるGHCのコンパイル速度も高められるはずです。それはそれでありがたい。

もう一つは、これまた例えば`HashMap`型のような木をベースにした連想配列も、配列ベースのハッシュテーブルに変換することができるのでしょうか？  
もしそうだとすると、ランダムアクセスに対する計算量のオーダーもO(log n)からO(1)に変わるわけですし、要素をまとめて処理する以外の演算についても劇的な改善が見込めるかもしれません。  
もちろんこれも先ほどの推測が正しければ無意味な想像ですが、夢のある話ですね。

Gibbonは将来的には、`Packed`という型クラスを提供することで、GHC本体への統合も視野に入れているそうです。  
`Packed`を実装した型は、値をどのように配列に変換するのか定義することで、Gibbonによる最適化のためのヒントを与えることができます。

参考: [木構造 (データ構造) - Wikipedia](https://ja.wikipedia.org/w/index.php?title=%E6%9C%A8%E6%A7%8B%E9%80%A0_\(%E3%83%87%E3%83%BC%E3%82%BF%E6%A7%8B%E9%80%A0\)&oldid=72655479)
