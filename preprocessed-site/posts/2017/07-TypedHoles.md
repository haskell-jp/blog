---
title: GHCのTyped Holes機能で、式中の部分の型を推論させる
headingBackgroundImage: ../../img/post-bg.jpg
headingDivClass: post-heading
postedBy: takenbu.hs
tags: Haskell-jp
date: August 8, 2017
...
---

最近はあまり話題になってないかもしれませんが、 `Typed holes` 機能についての紹介です。

ghc7.8くらいの頃に導入された機能で、ソースコードの式の中に `_` か `_` で始まる識別名を書くと、その部分の型を推論してくれます。

### 使い方

例えば、

```Haskell
main = print $ 1.0 + _
```
のようなコードを書いて、 `runghc`や`ghci`や`ghc`で実行すると、 `_` 部の型をエラーメッセージで教えてくれます。  


### エラーメッセージの表示例

例えば、以下のようにエラーメッセージが表示されます。 以下の例では、`_`部が、`Double`型であることを示しています。

```Haskell
$ runghc test1.hs 

test1.hs:3:22: error:
    • Found hole: _ :: Double
    • In the second argument of ‘(+)’, namely ‘_’
      In the second argument of ‘($)’, namely ‘1.0 + _’
      In the expression: print $ 1.0 + _
    • Relevant bindings include main :: IO () (bound at test1.hs:3:1)
  |
3 | main = print $ 1.0 + _
  |                      ^

```


### 補足1

`_` の替りに、 `_hoge` のように名前付けしても構いません。

ghci で、 `:t` で型を調べるのと同じように気軽に使えます。　（最近はIDEがサポートしていて、明示的には使わなかったりするかもしれません。）


### 補足2

詳細情報は以下にあります。

* <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#typed-holes>
