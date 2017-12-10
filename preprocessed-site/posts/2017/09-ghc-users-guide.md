---
title: GHCのユーザーズガイドへのリンク集
headingBackgroundImage: ../../img/post-bg.jpg
headingDivClass: post-heading
subHeading: コンパイル時オプション、実行時オプション、対話コマンド、言語拡張など
author: takenbu.hs
date: August 14, 2017
...
---

## はじめに

Haskell用コンパイラであるGHCのユーザーズガイド（マニュアル）の在り処について紹介します。
また、GHCのユーザーズガイドはボリュームが多いため、頻繁に調べる項目を見つけやすいように、いくつかの章や節の在り処を簡単に紹介します。



## ユーザーズガイドの在り処

GHCの最新版のユーザーズガイド（英語版）は以下にあります。

* [`Glasgow Haskell Compiler User's Guide: Web版`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/)
* [`Glasgow Haskell Compiler User's Guide: PDF版`](https://downloads.haskell.org/~ghc/latest/docs/users_guide.pdf)


なお、素晴らしい日本語翻訳版が以下にあります。

* [GHC 8.0](https://ghcguide.haskell.jp/users_guide/index.html)  (nobsunさん訳; 翻訳中)
* [GHC 7.8](http://www.kotha.net/ghcguide_ja/7.8.2/)  (kothaさん訳)



## ユーザーズガイド内で、よく調べる項目の在り処

GHCのユーザーズガイドはボリュームが多いため、頻繁に調べる項目を見つけやすいように、いくつかの章や節を以下に紹介します。

* [GHCのコンパイル時オプション（フラグ）一覧](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html)
* [GHCの実行時オプション（フラグ）](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html#setting-rts-options)
* [GHCのプロファイラのオプション](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#profiling)
* [GHCiのコマンド](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#ghci-commands)
* [runghcのフラグ](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runghc.html#runghc-flags)
* [GHCの言語拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#language-options)
* [GHCのプラグマ](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#pragmas)
* [ghc-pkgのコマンド（オプション）](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#using-packages)



## 補足１

### ユーザーズガイド内の検索について

ユーザーズガイドのWeb画面右側の"Quick search"の検索ボックスにより、ユーザーズガイド内の検索が可能です。


### ユーザーズガイドのソースの在り処について

* ユーザーズガイドのソースは、[こちら](https://github.com/ghc/ghc/tree/master/docs/users_guide)にあります。
* ユーザーズガイドの記述方法やビルド方法についての説明は、[こちら](https://ghc.haskell.org/trac/ghc/wiki/Commentary/UserManual)にあります。



## 補足２

参考までに、GHCではなく、Haskell言語仕様の方も以下に紹介します。

* [Haskell 2010 Language Report; Web版](https://www.haskell.org/onlinereport/haskell2010/)
* [Haskell 2010 Language Report; PDF版](https://www.haskell.org/definition/haskell2010.pdf)



