---
title: GHCのTyped Holes機能で、式中の部分の型を推論させる
headingBackgroundImage: ../../img/post-bg.jpg
headingDivClass: post-heading
postedBy: takenbu.hs
tags: Haskell-jp
date: Aug 7, 2017
...
---

最近はあまり話題になってないかもしれませんが、 `Typed holes` 機能についての紹介です。

ghc7.8くらいの頃に導入された機能で、ソースコードの式の中に `_` か `_` で始まる識別名を書くと、その部分の型を推論してくれます。

### 使い方

例えば、
```
main = print $ 1.0 + _
```
のようなコードを書いて、 `runghc xxx.hs` とすると、 `_` 部の型を教えてくれます。  
`_` の替りに、 `_hoge` のように名前付けしても構いません。

ghci で、 `:t` で型を調べるのと同じように気軽に使えます。　（最近はIDEがサポートしていて、明示的には使わなかったりするかもしれません。）


### 補足
詳細情報は以下です。
 * https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#typed-holes

