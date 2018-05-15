---
title: GHC拡張ノック(Part 1)
headingBackgroundImage: ../../img/post-bg.jpg
headingDivClass: post-heading
subHeading: n番煎じのよく使うGHC拡張の紹介
author: mizunashi_mana
postedBy: mizunashi_mana
tags: GHC, Language Extensions
date: May 15, 2018
...
---

Haskell[^notice-haskell-standard-version]では各処理系で言語拡張を提供し，`LANGUAGE`プラグマというものを利用することで，言語拡張を利用することが許容されています．Haskellのデファクト標準的な処理系GHCも多くの言語拡張を提供しており，その拡張は**GHC拡張**と呼ばれています．

[^notice-haskell-standard-version]: この記事では特に断らない限り，[Haskell2010][haskell-lang-report-url]を「Haskell標準」または「Haskell」と呼称します．

今回は，このGHC拡張の簡単な紹介と，個人的に良く使う拡張についての簡単な紹介を，全3回に分けて行いたいと思います．対象としては，GHCでHaskellプログラミングをしたことがあり，通常のHaskellの構文や動作方法が分かっている人を考えています．また，この記事はあくまで簡単な紹介に留めるもので，付随する留意点や詳細な機能説明は，大事な箇所は漏らさないよう注意するつもりですが，全てを網羅するつもりはありませんのでその点は注意してください．もし，実際にGHC拡張を使用する際は，[GHCのユーザーガイド][ghc-user-guide-url]をよく読んでから使用するのが良いでしょう．

# GHC拡張について

## Haskellの言語拡張

Haskellには，言語拡張を取り込む方法が標準で提供されています．Haskell標準では，コンパイラプラグマというものが策定されており，これを通してコンパイラに追加情報を提供することができます．コンパイラプラグマは`{-#`と`#-}`で囲まれ，字句的にはコメントとして扱われます．標準では，インラインプラグマや特殊化プラグマの他に，`LANGUAGE`プラグマというものが策定されており，このプラグマを通して言語拡張を指定することができます．

例えば，実装によって`CPP`と`ScopedTypeVariables`という名前の言語拡張が提供されており，それを使いたい場合，次のような文をモジュールの開始前に指定することで，言語拡張が有効になります．

```haskell
{-# LANGUAGE CPP, ScopedTypeVariables #-}

module A where
```

また，`LANGUAGE`プラグマを複数指定することもできます．

```haskell
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module A where
```

この機能を通して，多くのHaskell処理系では言語拡張を提供しています．

## GHC拡張

Haskellのデファクト標準な処理系GHCも，多数の拡張を提供しており，この拡張がGHC拡張と呼ばれるものです．GHC拡張は，バージョン8.4.2現在，以下の数が提供されています[^notice-supported-extensions-option]．

```bash
$ ghc --supported-extensions | wc -l
235
```

[^notice-supported-extensions-option]: このオプションは，拡張を無効にするGHC拡張(例えば，`NoImplicitPrelude`拡張など)も含めて表示します．実際には`No`が付いている拡張を抜くと，提供されている数は120個になります．

`--supported-extensions`オプションは，現在のGHCで使用できるGHC拡張を表示してくれるオプションです．ただ，GHC拡張は全てが独立した拡張ではなく，互いに依存しあった拡張が多く存在します．また，先頭に`No`がついている拡張は，そのGHC拡張を無効にするような拡張になっています [^notice-standard-disable-extensions] [^notice-both-disable-enable]（例えば，`NoImplicitPrelude`拡張は`ImplicitPrelude`拡張を無効にする拡張です)．

[^notice-standard-disable-extensions]: Haskell標準では，ある拡張を無効にするといった機能は提供されていません．このため，GHCでは無効にする機能を1つの拡張として，Haskell標準に則った形で提供しています．
[^notice-both-disable-enable]: 有効にする拡張と無効にする拡張を両方指定した場合，GHCは指定された順番に沿って最後に指定された方を拡張として採用します．

また，デフォルトで有効になっている拡張などもあります．例えば，`ImplicitPrelude`という拡張はデフォルトで有効になります．現在デフォルトのHaskell 2010をベースにしたモードでGHC 8.4.2を使用する場合，以下の拡張が[デフォルトで有効になります](https://github.com/ghc/ghc/blob/ghc-8.4.2-release/compiler/main/DynFlags.hs#L2022) [^notice-haskell2010-standard-exts] [^notice-default-extensions-by-ghc] [^notice-relaxed-polyrec]．

* [`NondecreasingIndentation`](https://prime.haskell.org/wiki/NondecreasingIndentation): Haskellのレイアウトルールを変更する拡張です．この拡張を有効にすると，ネストされた`do`式の場合，インデントをしなくていいようになります．
* [`ImplicitPrelude`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#rebindable-syntax-and-the-implicit-prelude-import): 暗黙的に`Prelude`モジュールがインポートされるようになる拡張です．
* [`MonomorphismRestriction`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#switching-off-the-dreaded-monomorphism-restriction): [単相性制限](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-930004.5.5)を課すようにする拡張です．この制限により，関数束縛でなく型注釈もない束縛変数の型は，デフォルティングルールによって単相化されます．
* [`TraditionalRecordSyntax`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#traditional-record-syntax): レコード構文を有効にする拡張です．この拡張では，名前付きのフィールドを持つデータ型を定義し，それを使用することが可能になります．
* [`EmptyDataDecls`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-EmptyDataDecls): コンストラクタを持たないデータ型の定義を許容する拡張です．
* [`ForeignFunctionInterface`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ffi-chap.html#extension-ForeignFunctionInterface): FFIが使えるようになる拡張です．この拡張により，`foreign import`構文を使用することで，HaskellからCの関数を読み込むことができるようになります．
* [`PatternGuards`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#pattern-guards): `case`式において，通常のパターンに加えて，`<-`を使用してガードの中でさらにマッチした条件下でパターンマッチができるようになる拡張です．例えば，`case (x, y) of { (True, y) | False <- y -> True; _ -> False }`というような式が書けるようになります．
* [`DoAndIfThenElse`](https://prime.haskell.org/wiki/DoAndIfThenElse): `if`式の構文を，`then`と`else`の前に`;`を許容するよう変更する拡張です．これにより，`do`式において`then`や`else`をインデントする必要がなくなります．

[^notice-haskell2010-standard-exts]: Haskell2010標準では，`Haskell2010`というプラグマをサポートすること，またHaskell98から新たにHaskell2010までに取り込まれた機能を切り離した`PatternGuards`/`NoNPlusKPatterns`/`RelaxedPolyRec`/`EmptyDataDecls`/`EmptyDataDecls`という拡張をそれぞれサポートすることが望ましいと規定されています．GHCも`Haskell2010`という拡張を指定できるようになっており，ここにあるほとんどはこの拡張を有効にした場合にも有効になります．
[^notice-default-extensions-by-ghc]: デフォルトで有効になる拡張のほとんどは，Haskell 2010を元にしたものです．ただし全てがそうというわけではありません．`NondecreasingIndentation`はHaskell標準にはない機能です．またGHCはHaskell 2010で規定されている仕様を全てデフォルトで取り込んでいる訳でもありません．特にHaskell標準ではデータ型の宣言に型制約を書くことができますが，GHCではデフォルトではできません．これを有効にする場合，[`DatatypeContexts`拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DatatypeContexts)を有効にする必要があります．
[^notice-relaxed-polyrec]: GHCの内部では`RelaxedPolyRec`という拡張も一緒に有効になります．しかし，現在この拡張は実装上の問題でGHC上で無効にすることができないため，ドキュメント上からも削除されています．この記事でもGHCの方針に従って，この拡張は特に扱いませんのでご留意ください．

歴史的経緯で生まれ，互換性のために残されているものの，現状使用が推奨されていない拡張もあります．他に実験的な拡張やかなり大胆な拡張も存在するため，GHC拡張を使用する際は[GHCのユーザーガイド][ghc-user-guide-url]をよく読んでから使用するのが良いでしょう．

## GHC拡張の使い方

GHCでGHC拡張を使用する方法は，Haskell標準の`LANGUAGE`プラグマを使用する他に，幾つかあります．まず，GHCにオプションを渡して有効にする方法です．例えば，`NoImplicitPrelude`拡張と`Strict`拡張を有効にした状態で`Main.hs`をコンパイルしたい場合，次のように書けます．

```bash
ghc -XNoImplicitPrelude -XStrict --make Main.hs
```

GHCでは`-X`の後に拡張名を続けることで，言語拡張を有効にしてコンパイルすることができます．通常は，`LANGUAGE`プラグマを使用するのが良いですが，何らかの事情で`LANGUAGE`プラグマを使用できない場合や，デフォルトで有効にしたい言語拡張がある場合などに便利でしょう．特にGHCiで言語拡張を有効にしたくなった場合，このオプションを`set`コマンドで指定すると良いでしょう．

```haskell
>>> :set -XNoImplicitPrelude -XStrict
```

他にGHC拡張を有効にする方法として，`Cabal`の機能を活用する方法があります．`cabal`ファイルのビルド情報欄には，[`default-extensions`というフィールド](https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-default-extensions)を指定することができ，そこにデフォルトで有効にしたい言語拡張のリストを書くことで，その拡張を有効にした状態で`Cabal`がビルドを行ってくれます．例えば，`NoImplicitPrelude`拡張と`Strict`拡張をデフォルトで有効にしてビルドしたい場合，次のように書きます．

```cabal
name:           TestPackage
version:        0.0
synopsis:       Small package with a program
author:         Angela Author
license:        BSD3
build-type:     Simple
cabal-version:  >= 1.2

executable program1
  build-depends:      base
  main-is:            Main.hs
  default-extensions: NoImplicitPrelude, Strict
```

# 主要なGHC拡張

以下では，個人的にデフォルトで有効化して使っている拡張を幾つか紹介します．なお，GHCのバージョンは8.4.2でHaskell2010モードで使用することを前提にしています．

## Preludeの暗黙的な使用を抑制する

この節では，以下の拡張を紹介します．

* `NoImplicitPrelude`: [ユーザーガイド - NoImplicitPrelude拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-NoImplicitPrelude)

Haskellでは，[Preludeモジュール][prelude-url]が暗黙的にimportされます．つまり，Haskellプログラムは暗黙に

```haskell
import Prelude
```

と書いてあると，解釈されるということです．[Preludeモジュール][prelude-url]には，`Int`/`IO`といった基本的なデータ型や，`Eq`/`Functor`といった基本的な型クラス，`zip`/`putStrLn`といった基本的な関数が含まれています．

[Preludeモジュール][prelude-url]の暗黙的なimportは，Haskellプログラムを簡潔に書く上では便利ですが，これを無効にしたい場合もあります．

1. [Preludeモジュール][prelude-url]にあるデータ型や関数と同じ名前の，別モジュールの関数を使いたい時
1. 別の代替となるpreludeパッケージを使う時

といった場合です．`NoImplicitPrelude`拡張はまさしくこのような場合に，[Preludeモジュール][prelude-url]を暗黙的にimportしないようにするGHC拡張です．1番目の理由の場合，この拡張をデフォルトで入れずモジュール度に指定すればいいと思いますが，私的には2番目の理由でこの拡張を使うためデフォルトで有効にしています．代替となるpreludeパッケージは幾つか存在しますが，主に

* classy-prelude: [Hackageリンク](https://hackage.haskell.org/package/classy-prelude)
* protolude: [Hackageリンク](https://hackage.haskell.org/package/protolude)
* universum: [Hackageリンク](https://hackage.haskell.org/package/universum)
* basic-prelude: [Hackageリンク](https://hackage.haskell.org/package/basic-prelude)

などがあります[^notice-rio]．これらのパッケージを探すには[HackageのPreludeカテゴリ](https://hackage.haskell.org/packages/#cat:Prelude)を参照するといいでしょう．

[^notice-rio]: 現在，Preludeの代替を目指す，[rio](https://hackage.haskell.org/package/rio)というパッケージが作成されています．このパッケージは現在まだprereleaseの段階で，[stack](https://github.com/commercialhaskell/stack)において実験的に使用されています．様々な最新のHaskellプログラミングの知見を取り入れており，標準のPreludeに大きく拡張を施しているため，Haskellで大規模な開発を行う場合注目する価値があるかもしれません．

私の場合，classy-preludeを使っていますが，それも生で使用しているわけではなく，パッケージごとにpreludeモジュールを作って使用しています．Preludeは，最もよく使うものが提供されているモジュールですから，APIの変更の影響を最も強く受けます．それを外部パッケージに依存させると，パッケージ保守が結構大変です．もし，パッケージごとにpreludeモジュールを作っておけば，パッケージ側やGHCのバージョン変更の影響などでAPIが変更されても，そのモジュール内でフォールバックを設定することで他のモジュールに変更を持ち越す必要がなくなります．これを`NoImplicitPrelude`拡張と組み合わせ，

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

module A where

import MyPrelude

...
```

と書くことで，保守がかなりしやすくなります．

## 便利な構文の導入

### 新たなリテラル表記を可能にする

この節では，以下の3つの拡張を紹介します．

* `BinaryLiterals`: [ユーザーガイド - BinaryLiterals拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-BinaryLiterals)
* `NagativeLiterals`: [ユーザーガイド - NagativeLiterals拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-NegativeLiterals)
* `HexFloatLiterals`: [ユーザーガイド - HexFloatLiterals拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XHexFloatLiterals)

Haskellには幾つかのリテラルが存在します．例えば，`'c'`は文字cを表すChar型のリテラルです．`100`は整数100を表す`Num a => a`型のリテラルで，`100.1`は浮動小数点数100.1を表す`Fractional a => a`型のリテラルになります．Haskell標準には他にも幾つかリテラルが存在しますが，特に数値は非常に多様な使われ方がなされるため，他の多くの言語はより強力なリテラル表現を持つことがあります．GHC拡張ではこの背景を元に，リテラルに対する幾つかの拡張を提供しています．`BinaryLiterals`は`Num a => a`型のリテラルに対して，`HexFloatLiterals`は`Fractional a => a`型のリテラルに対して，`NegativeLiterals`はどちらに対してもの拡張を，それぞれ提供します．

数値型に対するリテラルは，既存のものでも数種類存在します．通常の数値表現`20`，オクテット(8進数)表現`0o24`，ヘックス(16進数)表現`0x14`の3つです．`BinaryLiterals`拡張は，これに加え`0b`を接頭辞に付けることでバイナリ(2進数)表現`0b10100`を可能にする拡張です．

これらのオクテット表現やヘックス，バイナリ表現は浮動小数点数の表現はできません．しかし，浮動小数点数は実際にはIEEEの規格に則ったデータ表現になりますから，10進数表現よりも16進数表現の方が実態として分かりやすい場合があります．このため`HexFloatLiterals`拡張では，接頭に`0x`の付くヘックス表現でも浮動小数点数のリテラルを記述できるようにしています．この拡張によって，`0.25`は`0x0.4`と表記できるようになります．また，指数表記も10進方式のものではなく，ビット方式のものになります．指数表記には`e`ではなく`p`を使い，何ビット移動させるか(つまり，2の何乗を掛けるか)を書くようにします．例えば，`1.0`は`0x0.4p2`と表記できます．また，`0.125`は`0x0.4p-1`と表記できます．

さて，Haskellには唯一の単項演算子`-`があります．この演算子を使用することで`negate 1`の代わりに`-1`という表記が可能になります．しかし，この演算子の結合度は非常に弱く，また二項演算子の`-`も存在することから`f -1`という表記は`(f) - (1)`というように解釈されてしまうなどの問題があり，非常に使い勝手が悪い演算子となっていました．また，Haskellの仕様上，`-128`という表現は最終的に`negate (fromInteger 128)`という式に[脱糖されます](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-280003.4)が，例えば`Int8`などの，負数は`-128`まで扱えるが正数は`+127`までしか扱えないといったデータ型の場合に，この式は`fromInteger`で一度`+128`の値になってしまいオーバーフローを起こしてしまうという問題がありました．これを解決するため導入されたのが`NagativeLiterals`拡張です．この拡張を導入することで空白を挟まない`-1.0`などは1つのリテラルと解釈されるようになります．この拡張を導入後は，次のようになります．

```haskell
>>> max -1 2 == max (-1) 2 -- before: max -1 2 == max - (1 2)
True
>>> data SamplePZ = SamplePZ deriving (Eq, Show)
>>> instance Num SamplePZ where { fromInteger i | i <= 0 = SamplePZ }
>>> -100 :: SamplePZ -- before: raise error
SamplePZ
>>> - 100 :: SamplePZ
*** Exception: ...
>>> instance Fractional SamplePZ where { fromRational r | r <= 0 = SamplePZ }
>>> -100.10 :: SamplePZ -- before: raise error
SamplePZ
>>> - 100.10 :: SamplePZ
*** Exception: ...
```

### 空のデータ型に対するより強力なサポートを導入する

この節では，以下の2つの拡張を紹介します．

* `EmptyCase`: [ユーザーガイド - EmptyCase拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-EmptyCase)
* `EmptyDataDeriving`: [ユーザーガイド - EmptyDataDeriving拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XEmptyDataDeriving)

Haskellでは，コンストラクタを一切持たない型を定義できます．これは空のデータ型と呼ばれ，次のように書けます．

```haskell
data Empty
```

このような型は`base`パッケージの`Data.Void`モジュールでも提供されており，有用な場合があります．しかし，Haskell標準ではこのようなデータ型に対するサポートが薄く，使用する上で不便な場面があります．このサポートを強化する拡張が，`EmptyCase`拡張と`EmptyDataDeriving`拡張です．

`EmptyCase`拡張は，空のパターンマッチを書けるようにする拡張です．Haskell標準では，空のパターンマッチは書けません．つまり，`case x of {}`というような式が書けないということです．通常はデータ型は何らかのコンストラクタを持っていますから，このようなパターンマッチを書きたいと思う場面はないでしょう．しかし，空のデータ型においてこのようなパターンマッチを書きたいと思うことがあります．

```haskell
f :: Empty -> a
f x = case x of {}
```

このような表記を可能にするのが`EmptyCase`拡張です．なお，このケース式は次のように書くのと同値になります．

```haskell
f :: Empty -> a
f x = x `seq` error "Non-exhaustive patterns in case"
```

もう1つの`EmptyDataDeriving`拡張は，空のデータ型に対して`deriving`構文を使用できるようにする拡張です．空のデータ型は，通常のデータ型と違い`Eq`や`Show`などの型クラスインスタンスを[`deriving`することができません](https://www.haskell.org/onlinereport/haskell2010/haskellch11.html#x18-182014x6)．つまり以下のようなことができません．

```haskell
data Empty
  deriving (Eq, Ord, Show)
```

しかし，これでは不便な場合があります．それを可能にするのが`EmptyDataDeriving`拡張です．この拡張では，`Eq`/`Ord`/`Show`/`Read`の4つが`deriving`可能になり，それぞれは次のようなインスタンスを生成します．

```haskell
instance Eq Empty where
  _ == _ = True

instance Ord Empty where
  compare _ _ = EQ

instance Read Empty where
  readPrec = pfail

instance Show Empty where
  showsPrec _ x = case x of {}
```

### 新たな基本構文を導入する

この節では，以下の3つの拡張を紹介します．

* `TupleSections`: [ユーザーガイド - TupleSections拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TupleSections)
* `MultiWayIf`: [ユーザーガイド - MultiWayIf拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-MultiWayIf)
* `LambdaCase`: [ユーザーガイド - LambdaCase拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-LambdaCase)

Haskellでは，タプルやラムダ抽象，セクション，`if`式や`case`式といった構文が導入されていますが，これらを組み合わせて多用する場合，幾つか冗長な表現が生まれる場合があります．その中でも頻出する表現に対して，新たな構文を提供するGHC拡張があります．それが，`TupleSections`，`MultiWayIf`，`LambdaCase`の3つの拡張です．

Haskellには，セクションと呼ばれる二項演算子の部分適用を表す構文があります．また，Haskellではタプルにも独自の構文が充てがわれています．このタプルを使用する際，セクションのように部分適用を簡潔に書きたい場合があります．例えば，`\x -> (1, x)`という表現をもっと簡潔に書きたい場合があります．この場合は`(,) 1`というな表記が可能ですが，2番目に部分適用したい場合や，3つ組のタプルに部分適用したい場合などは非常に面倒です．このため，`TupleSections`拡張は`(1, )`という表記でタプルの部分適用を書ける構文を提供します．2つ以上空きがある場合は，左から引数を受け取っていくようになります．例えば，`(True, , "str", )`は`\x y -> (True, x, "str", y)`と同等です．

`MultiWayIf`は名前の通り複数の条件をガード構文のように指定できる`if`式を提供する拡張です．つまり，以下のようなことがかけます．

```haskell
f :: [Int] -> IO ()
f xs = sequence_ $ do
  x <- xs
  pure $ if
    | x <= 0          -> fail "non-positive number"
    | x `mod` 15 == 0 -> putStrLn "FizzBuzz"
    | x `mod` 3  == 0 -> putStrLn "Fizz"
    | x `mod` 5  == 0 -> putStrLn "Buzz"
    | otherwise       -> print x
```

この`MultiWayIf`は次のように`case`式で書き換えることが可能です．

```haskell
f :: [Int] -> IO ()
f xs = sequence_ $ do
  x <- xs
  pure $ case () of
    _ | x <= 0          -> fail "non-positive number"
    _ | x `mod` 15 == 0 -> putStrLn "FizzBuzz"
    _ | x `mod` 3  == 0 -> putStrLn "Fizz"
    _ | x `mod` 5  == 0 -> putStrLn "Buzz"
    _ | otherwise       -> print x
```

3つ目の`LambdaCase`拡張は，ラムダ抽象と`case`式を組み合わせた際に良く使う表現をより簡潔に書けるようにする拡張です．この拡張を使うと，`\x -> case x of (a, b) -> a + b`というようなラムダ抽象を，`\case (a, b) -> a + b`と書けるようになります．もちろんレイアウトルールも`case-of`式と同じように作用するため，改行を含んだ式も書けます．

```haskell
f :: Maybe Int -> Int
f = negate . \case
  Nothing -> 0
  Just x  -> x
```

### 正格化に対するサポートを導入する

この節では，以下の3つの拡張を紹介します．

* `BangPatterns`: [ユーザーガイド - BangPatterns拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-BangPatterns)
* `StrictData`: [ユーザーガイド - StrictData拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-StrictData)
* `Strict`: [ユーザーガイド - Strict拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-Strict)

Haskellはデフォルトの評価戦略として，グラフ簡約の遅延評価を採用しています．これはリストや再帰に関する表現を非常に豊かにする反面，パフォーマンスを悪化させたりデバッグを困難にさせる場面が多いなどの負の面もあります．このためHaskell標準では，`seq`関数や正格フラグといった正格評価へのサポートも提供しています．しかし，このサポートは表現が冗長な場合が多く，使い勝手が悪い側面があります．この面を解決するための拡張が，`BangPatterns`，`StrictData`，`Strict`の3つの拡張です．

再帰関数において，累積引数は多くの場合正格に計算した方が効率が良いですが，Haskell標準では以下のように書く必要がありました．

```haskell
sum :: [Int] -> Int -> Int
sum xs y = y `seq` case xs of
  x:xs' -> sum xs' (x + y)
  []    -> y
```

このような`seq`による評価をより簡潔に書けるよう，`BangPatterns`拡張というものが提供されています．これはパターンを拡張し，バンパターンというものを導入します．このバンパターンは，通常のパターンに`!`を付けることで書けます．例えば，上の例はバンパターンを使うと以下のように書けます．

```haskell
sum :: [Int] -> Int -> Int
sum xs !y = case xs of
  x:xs' -> sum xs' (x + y)
  []    -> y
```

バンパターンはパターンの1つですから，もちろん`let`式や`case`式でも`let !y = f x in y`や`case f x of !y -> y`というように使えます．また，`case x of (!y, z) -> y + z`というように部分パターンとしても有効です．バンパターンは[Haskellの`case`式の翻訳ルール][formal-semantics-of-pattern-matching]に次の規則を加えることで実現されます．

```haskell
case v of { !pat -> e; _ -> e' }
≡ v `seq` case v of { pat -> e; _ -> e' }
```

Haskell標準では，データ型の宣言において，コンストラクタの引数に正格フラグというものを付けることが許容されています．このフラグをつけた引数は，正格に評価された後コンストラクタに渡されます．ただ，一般にデータ型の引数は正格な方が効率が良いため，データ型宣言時に正格フラグを付けるという慣習がありました．この慣習を打破するために導入されたのが，`StrictData`拡張です．`StrictData`拡張下のモジュールでは，データ型宣言時，コンストラクタの引数は全て正格フラグをつけているものとして扱われます．また，`~`というフラグが新たに導入され，このフラグをつけた引数の場合はHaskell標準化のデフォルトの動作，つまり引数は正格に評価されず遅延されるようになります．`StrictData`下で宣言された

```haskell
data T = Normal Int | Strict !Int | Lazy ~Int
```

というデータ型は，通常のHaskellの以下のデータ型と同等になります．

```haskell
data T = Normal !Int | Strict !Int | Lazy Int
```

`Strict`拡張は，`StrictData`拡張に加え，ほとんどのパターンを暗黙的にバンパターンにする拡張です．つまり，殆どの評価を正格にする拡張です．バンパターンに変わる箇所は，関数の引数，`let`/`where`句の束縛変数，`case`式のパターンマッチなどです．これらのパターンには，最外の場所に`!`が暗黙的に付与されます．例えば，`Strict`拡張下で定義された

```haskell
f :: Int -> (Int, Int) -> Int
f x (z, y) = let zy = z * y in case x - z of z' -> z' ^ z
```

という関数は，`BangPatterns`拡張下のHaskellの以下の関数と同等になります．

```haskell
f :: Int -> (Int, Int) -> Int
f !x !(z, y) = let !zy = z * y in case x - z of !z' -> z' ^ z
```

注意して欲しいのは，このバンパターンは`seq`に置き換わるため，WHNFまでしか評価されないということです．つまり，`!(z, y)`というパターンは単なる`(z, y)`と完全に同じです．またトップレベルの束縛にバンパターンを付与することは許されておらず，遅延されるということにも注意が必要です．

### パターンマッチをより柔軟に扱えるようにする

この節では，以下の2つの拡張を紹介します．

* `ViewPatterns`: [ユーザーガイド - ViewPatterns拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ViewPatterns)
* `PatternSynonyms`: [ユーザーガイド - PatternSynonyms拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-PatternSynonyms)

GHC拡張では，Haskell標準のパターンをさらに強力なものにする拡張があります．`ViewPatterns`はビューパターンという新たなパターンを導入します．また，`PatternSynonyms`はパターンの別名を付けることができるようにする拡張です．

Haskell標準にあるパターンガードは，非常に強力ですが，表現が非常に冗長になる場合があります．これを短縮して書けるように，`ViewPatterns`拡張はビューパターンというものを導入します．ビューパターンは，`->`の左側に式を，右側にパターンを書くことで，左の式に対象を適用して結果が右側のパターンにマッチした時，マッチするようなパターンです．例えば，

```haskell
f ((`mod` 2) -> 0) = Nothing
f x                = Just x
```

というように使用でき，`f 0`は`Nothing`を，`f 3`は`Just 3`をそれぞれ返すようになります．この関数宣言は，以下のパターンガードを用いて書いた関数と一致します．

```haskell
f x | 0 <- x `mod` 2 = Nothing
f x                  = Just x
```

ビューパターンは[Haskellの`case`式の翻訳ルール][formal-semantics-of-pattern-matching]に次の規則を加えることで実現されます．

```haskell
case v of { (e -> p) -> e1; _ -> e2 }
≡ case (e v) of { p -> e1; _ -> e2 }
```

`PatternSynonyms`拡張は，非常に強力で大きな拡張です[^notice-pattern-synonyms-bugs]．`PatternSynonyms`拡張は名前の通り，パターンに別名を与えるパターンシノニム機能を提供します．パターンシノニムは通常の関数と同じように，次のように定義できます．

[^notice-pattern-synonyms-bugs]: GHC 8.2.2の段階では，パターンシノニムはコンパイラがクラッシュするなどの非常に多くのバグを抱えていました．私は8.4.2をまだあまり試していませんが，パターンシノニムの仕様が非常に複雑なため，8.4.2でもまだバグを多く抱えている可能性があります．パターンシノニムをプロダクトで多用する場合，その点に注意した方が良いでしょう．

```haskell
pattern Nil :: [a]
pattern Nil = []

pattern Cons :: a -> [a] -> [a]
pattern Cons x xs = x : xs

{-# COMPLETE Nil, Cons #-}
```

このように定義したパターンは，以下のように使用できます．

```haskell
len :: [a] -> Int
len (Cons _ xs) = 1 + len xs
len Nil         = 0
```

パターンシノニムは非常に便利な機能ですが，一方で注意する事項も幾つかあります．

まず，パターンシノニムの定義は関数定義と非常に似ていますが，パターンの別名であることに注意してください．パターンシノニムの定義において変数が出現する場合，関数の引数のように錯覚してしまいがちですが，この変数にはパターンにマッチした時そのマッチした部分が当てがわれます．つまり，右の式でマッチしたものが左の変数に束縛されるため，左の変数に束縛された後右の式を実行する関数と，流れが逆になるということです．このため，パターンシノニムの引数の変数は必ず右に出現する必要があります．また，パターンシノニムの右側には変数を含むパターンしかかけません．そのため，式を書きたい場合，`ViewPatterns`拡張などを用いなければなりません．さらにパターンシノニムは，デフォルトではパターンの網羅性検査が非常に難しいため，網羅性検査を行わないようになっています．ただし，[`COMPLETE`プラグマ](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#complete-pragma)を用いてパターンシノニムの網羅条件を与えることで，その範囲で網羅性検査を行うようになります．

パターンシノニムはパターンの種類に応じて3種類の書き方が存在します．上の単純なパターンシノニムは，双方向(bidirectional)パターンシノニムと呼ばれ，暗黙的にパターンの名前と等しい関数が作られます．この関数を用いることで，`[0, 1, 2]`の代わりに`Cons 0 (Cons 1 (Cons 2 Nil))`といった式も書くことができるようになります．ただし，このような関数が単純には作れないパターンも存在します．例えば，`(x, _)`というパターンに，`First x`というパターンシノニムを与えたい場合，この`First`に対する関数は`_`の部分に入れるべき値が分からないため，作りようがありません．このような関数が単純に作れないパターンシノニムは単方向(unidirectional)パターンシノニムと呼ばれ，双方向パターンシノニムが`=`を使って定義されるのに対し，次のように`<-`を使って書きます．

```haskell
pattern First :: Int -> (Int, Bool)
pattern First x <- (x, _)
```

このパターンシノニムは`First`という関数は作らず，単純にパターンの別名だけを提供します．ただし，`First`関数の定義を次のように与えることが可能になっています．

```haskell
pattern First :: Int -> (Int, Bool)
pattern First x <- (x, _)
  where
    First x | x < 0 = (x, False)
    First x         = (x, True)
```

また，パターンシノニムはパターンの評価順序にも注意する必要があります．例えば，次の例をみてください．

```haskell
data Pair a b = Pair a b

type Pair3 a b c = Pair a (Pair b c)

pattern Pair3 :: a -> b -> c -> Pair3 a b c
pattern Pair3 x y z = Pair x (Pair y z)

f :: Pair3 Bool Bool Bool -> Bool
f (Pair3 True True True) = True
f _                      = False

f' :: Pair3 Bool Bool Bool -> Bool
f' (Pair True (Pair True True)) = True
f' _                            = False
```

この`f`と`f'`は評価順が異なり，`f (Pair False undefined)`が例外を投げるのに対し，`f' (Pair False undefined)`は`False`を返します．これは，パターンシノニムを使ったパターンマッチでは，自身のパターンを先に調べ，次に引数のパターンマッチを行うからです．つまり，`f`は以下と同等になります．

```haskell
f :: Pair3 Bool Bool Bool -> Bool
f (Pair x (Pair y z)) | True <- x, True <- y, True <- z = True
f _                                                     = False
```

パターンシノニムは，モジュールエクスポートを書く際にも注意が必要で，`module A (pattern Cons, pattern Nil) where ...`というように接頭に`pattern`をつける必要があります．

### レコードに対するサポートを強化する

この節では，以下の4つの拡張を紹介します．

* `DuplicateRecordFields`: [ユーザーガイド - DuplicateRecordFields拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DuplicateRecordFields)
* `OverloadedLabels`: [ユーザーガイド - OverloadedLabels拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-OverloadedLabels)
* `NamedFieldPuns`: [ユーザーガイド - NamedFieldPuns拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-NamedFieldPuns)
* `RecordWildCards`: [ユーザーガイド - RecordWildCards拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RecordWildCards)

Haskellのレコード構文は，便利な反面幾つか機能が劣る場面もあります．このため，GHCでは，レコードをより扱いやすくするための拡張を幾つか提供しています．それが，`DuplicateRecordFields`，`OverloadedLabels`，`NamedFieldPuns`，`RecordWildCards`の4つの拡張です[^notice-record-systems-bugs]．

[^notice-record-systems-bugs]: GHCのレコードシステムの拡張は非常に強力ですが，その反面システムが非常に複雑になっています．このため，8.2.2の段階でコンパイラがクラッシュするなど非常に多くのバグを抱えていました．レコードシステムの仕様の改良は現在も進んでいますが，8.4.2でもまだバグを多く抱えている可能性があります．これらの拡張をプロダクトで多用する場合，その点に注意した方が良いでしょう．特に，GHC 8.0以降に導入された拡張には注意が必要です．

Haskell標準では，同じモジュール内で同じフィールド名を持つ複数のレコード構文を使用したデータ型の定義を行うことができません．これはどのデータ型のフィールドかが曖昧であるようなプログラムを書けてしまうからですが，そういう状況に遭遇するとこの制約は非常に不便です．これを解決するのが，`DuplicateRecordFields`拡張です．`DuplicateRecordFields`拡張は，曖昧になるような式を書けなくする代わりに，同一モジュールの複数のデータ型が同じフィールド名を持つことを許容する拡張です．つまり，以下のようなことが可能になります．

```haskell
data A = A { d :: Int }
data B = B { d :: Bool }
```

ただし，この拡張下では，曖昧なフィールドを用いたレコードのアップデート構文やフィールドの選択関数の使用の際は型を明記する必要があったり，モジュールのエクスポートリストで選択関数をエクスポートすることが出来なくなったりします．

`OverloadedLabels`拡張は，`#foo`というような`#`から始まる新たな構文を導入します．`#foo`は`GHC.OverloadedLabels`モジュールの`fromLabel`メソッドにおいて`IsLabel "foo" a => a`というような型を持つ場合と同等になります．これを用いることで，同じフィールドを持つデータ型に対する選択関数を次のように書けます[^notice-ghcexts-for-overloaded-record-selector]．

[^notice-ghcexts-for-overloaded-record-selector]: `OverloadedLabels`拡張はかなり最近入った拡張で，多数のGHC拡張，特に強力な型システムを前提にして書かれています．このため，選択関数の実装にもかなり多くのGHC拡張を使用しています．ここでは，特に解説しないのでそういうものだと思っておいてください．なお，このプログラムはプロダクションで使うことを前提にしていませんので，そこはご注意ください．

```haskell
{-# LANGUAGE OverloadedLabels       #-} -- the main extension
{-# LANGUAGE DataKinds              #-} -- for Symbol kind
{-# LANGUAGE KindSignatures         #-} -- for HasField's `l` parameter
{-# LANGUAGE MultiParamTypeClasses  #-} -- for HasField and IsLabel classes
{-# LANGUAGE FunctionalDependencies #-} -- for HasField class
{-# LANGUAGE FlexibleInstances      #-} -- for HasField instances
{-# LANGUAGE ScopedTypeVariables    #-} -- for the IsLabel instance
{-# LANGUAGE DuplicateRecordFields  #-} -- for A and B data types

import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (Symbol)
import Data.Proxy (Proxy(..))

data A = A { d :: Int }
data B = B { d :: Bool }

class HasField a (l :: Symbol) b | a l -> b where
  selectField :: Proxy l -> a -> b

instance HasField A "d" Int where
  selectField _ (A x) = x

instance HasField B "d" Bool where
  selectField _ (B x) = x

instance HasField a l b => IsLabel l (a -> b) where
  fromLabel = selectField (Proxy :: Proxy l)
```

これを使うことで，`#d A { d = 0 }`は`0`を，`#d B { d = True }`は`True`を返してくるようになります．また，`#d`には型を明記しなくても型推論が働くようになります．

さて他にレコードのパターンマッチやコンストラクトを非常に便利にしてくれる拡張として，`NamedFieldPuns`拡張と`RecordWildCards`拡張があります．レコードのパターンマッチは多くの場合冗長になりがちで，次のようなボイラープレートを書きがちです．

```haskell
data A = A { x :: Int, y :: Bool }

f :: A -> Int
f A{ x = x } = x + 1
```

`NamedFieldPuns`拡張は，同等のことを次のように書けるようにする拡張です．

```haskell
f :: A -> Int
f A{ x } = x + 1
```

また，このパターンは旧来の書き方と合わせて書くこともできます．

```haskell
g :: A -> Int
g A{ x, y = False } = - x
g A{ x }            = x
```

さらにこの拡張は，コンストラクトの際も役に立ちます．`let x = 1 in A { x, y = True }`と書くとこの式は，`A { x = 1, y = True }`と書くのと同等になります．

`NamedFieldPuns`拡張ではフィールド名を明記する必要がありましたが，`RecordWildCards`拡張はさらにフィールド名を明記する必要がなくなります．以下のように`{..}`と書くことで，全てのフィールドを展開してくれるようになります．

```haskell
f :: A -> Int
f A{..} = x + 1
```

また，部分的に明記することも可能で，その場合以下のように書きます．

```haskell
g :: A -> Int
g A{ y = False, ..} = -x
g A{..}             = x
```

コンストラクトの際も，この拡張は有効です．`let x = 1 in A { y = True, ..}`と書いた場合，`A { x = 1, y = True }`と書くのと同等になります．

### 型演算子を導入する

この節では，以下の拡張を紹介します．

* `TypeOperators`: [ユーザーガイド - TypeOperators拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeOperators)

Haskellではユーザー定義の関数やデータ型のコンストラクタにおいて，演算子表記のものも定義できるようになっています．例えば，以下のようにです．

```haskell
data Pair a b = a :*: b
infixl 7 :*:

(&) :: a -> (a -> b) -> b
x & f = f x
infixl 1 &
```

しかしHaskell標準では，型を定義する場合そのようなことはできません．これを可能にするのが，`TypeOperators`拡張です．この拡張の有効下では，

```haskell
type a + b = Either a b
infixr 5 +
```

ということが可能になります．ただし，このように定義した型演算子は，同じ名前の値としての演算子があった場合区別ができません．このため，モジュールのエクスポートリストを書く際，型演算子か値レベルの演算子かの区別が付かなくなった場合，値レベルの方が優先されます．この時，型演算子を明示したい場合，`type`を付けます[^notice-explicit-namespaces]．

[^notice-explicit-namespaces]: この機能は型演算子を定義しないで再エクスポートなどをする場合にも使用されるため，[`ExplicitNamespaces`拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ExplicitNamespaces)として切り離されています．

```haskell
{-# LANGUAGE TypeOperators #-}

module A
  ( type (+)
  ) where

type a + b = Either a b
```

### 型クラスを拡張する

この節では，以下の4つの拡張を紹介します．

* `MultiParamTypeClasses`: [ユーザーガイド - MultiParamTypeClasses拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-MultiParamTypeClasses)
* `FlexibleContexts`: [ユーザーガイド - FlexibleContexts拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FlexibleContexts)
* `FlexibleInstances`: [ユーザーガイド - FlexibleInstances拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FlexibleInstances)
* `InstanceSigs`: [ユーザーガイド - InstanceSigs拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-InstanceSigs)

Haskellの型クラスは非常に強力な機構です．しかしながら，Haskell標準の型クラスの構文は非常に制約がきつく，これらを緩和したいと思うことがよくあります．このため，GHCでは制約を緩和する拡張をいくつか提供しています．それが，`MultiParamTypeClasses`，`FlexibleContexts`，`FlexibleInstances`，`InstanceSigs`の4つの拡張です．

Haskell標準では，クラスは1つの変数しか持てません．なので，次のような型クラスは作れません．

```haskell
class C a b
```

これは非常に不便な制約なため，複数のパラメータを使うような型クラスを許容する拡張が`MultiParamTypeClasses`拡張です．この拡張により，上のコードが許容されるようになる他，以下のように変数が全くない型クラスも宣言することができるようになります．

```haskell
class Nullary
```

また，Haskell標準では，メソッドにおいてクラスの型変数に型制約をかけるということも許容されていませんが，`MultiParamTypeClasses`拡張ではこれも可能にします[^notice-constrained-class-methods]．これによって以下のようなクラス定義も書けるようになります．

[^notice-constrained-class-methods]: この機能は[`ConstrainedClassMethods`拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ConstrainedClassMethods)として切り離されており，`MultiParamTypeClasses`拡張を有効にすると一緒に有効になります．

```haskell
class Setable s a where
  elem :: Eq a => a -> s a -> Bool
```

Haskell標準では，型制約の解決を安全に，しかも単純にするために，[型注釈における制約の書き方](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-660004.1.3)や[クラス定義，インスタンス定義の際の制約の書き方](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-760004.3.1)を大きく制限しています．しかし，より複雑な型制約を書きたい時が往々にしてあります．そこで，この制限を緩め，クラス階層が非循環である場合には許容するようにする拡張が，`FlexibleContexts`拡張です．この拡張下では，

```haskell
-- valid
class (Monad m, Monad (t m)) => Transform t m where
  lift :: m a -> (t m) a

-- valid
f :: Functor Maybe => ()
f = ()

-- invalid
class A a => B a
class B a => A a
```

となります．

`FlexibleInstances`拡張も`FlexibleContexts`拡張と同じく，Haskell標準での[型クラスインスタンスの書き方](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-770004.3.2)の制限を，[停止制限](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#instance-termination)を守る場合に許容するというように緩和する拡張です．停止制限は簡単に言ってしまえば，インスタンス宣言において，型制約がインスタンスより小さく[^notice-smaller-constraint]，型関数を使っていないというものです[^notice-instance-termination-rule]．この拡張下では，

[^notice-smaller-constraint]: 型制約が小さいとは，型変数とコンストラクタと変数の組の出現が少ないということです．
[^notice-instance-termination-rule]: より正確には，`FunctionalDependencies`に対する制限もありますが，ここでは割愛します．

```haskell
-- valid
instance C1 (Maybe [a])

-- valid
instance C2 a a => C2 [a] [a]

-- valid
instance (Eq a, Show b) => C3 a b

-- valid
instance (Show a, Show (s a)) => Show (S s a)

-- invalid
instance C4 a => C4 a

-- invalid
instance C2 a a => C1 [a]

-- invalid
instance Functor [] => C1 a
```

となります．また，この拡張下では，型シノニムをインスタンスにすることもできます[^notice-type-synonym-instances]．

[^notice-type-synonym-instances]: この拡張は，[`TypeSynonymInstances`拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeSynonymInstances)として切り離されており，`FlexibleInstances`拡張を有効にすると一緒に有効になります．

```haskell
type List a = [a]

-- Instead of `instance C [a]`
instance C (List a)
```

ただし，型シノニムを使う場合そのシノニムの引数は全て適用しなければならないことに注意が必要です．

Haskell標準では，型クラスインスタンスの定義時，そのメソッドの型注釈は書けないようになっています．しかし，複雑な型クラスインスタンスを書く際，メソッドの型注釈を書きたい場合があります[^notice-instance-sigs-for-scoped-type-variables]．これを可能にするのが`InstanceSigs`拡張です．`InstanceSigs`拡張の元では，以下のようなインスタンス宣言が書けます．

[^notice-instance-sigs-for-scoped-type-variables]: 特に`ScopedTypeVariables`拡張を指定する場合，型注釈は必要です．

```haskell
data A = A

instance Eq A where
  (==) :: A -> A -> Bool
  A == A = True
```

### 型ワイルドカードをより柔軟に扱う

この節では，以下の拡張を紹介します．

* `NamedWildCards`: [ユーザーガイド - NamedWildCards拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-NamedWildCards)

GHCには型ワイルドカードという機能があります．この機能は，`_`と型シグネチャ上で書いておくと，そこの部分の型を推論してエラーメッセージとして表示してくれる機能です．この機能は，以下のように部分的に記述したり複数指定したりすることも可能です．

```haskell
-- Inferred type: (a, b) -> (a, Maybe a1)
ignoreSecond :: _ -> _
ignoreSecond (x, _) = (x, Nothing)
```

これを活用すれば，複雑な型をある程度ヒントを与えた状態で推論してもらい，型を追記するプログラミングスタイルや，GHCが実際に型をどう推論するかを見るための補助に応用できます．しかし，例えば`ignoreSecond`が引数と返り値で型が同じであるという情報が分かっていた場合に，これをヒントとして伝えたい場合がありますが，型ワイルドカードでそれを伝える方法はありません．これを解決するのが`NamedWildCards`拡張です．この拡張を使うと，以下のようなプログラムに対しても，接頭に`_`が付いている型をワイルドカードとみなして，エラーメッセージで型の推論結果を表示してくれるようになります．

```haskell
-- Inferred type: (a, Maybe a1) -> (a, Maybe a1)
ignoreSecond :: _a -> _a
ignoreSecond (x, _) = (x, Nothing)
```

### 新たな表記法の導入

この節では，以下の2つの拡張を紹介します．

* `Arrows`: [ユーザーガイド - Arrows拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-Arrows)
* `RecursiveDo`: [ユーザーガイド - RecursiveDo拡張](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RecursiveDo)

Haskellでは，モナドを扱いやすくするための，do構文という専用の構文が用意されています．この構文はHaskellプログラミングにおいて広く利用されています．GHCでは，これに加え`Arrow`と`MonadFix`というクラスに対しての専用の構文も提供しています．これはGHC拡張で実装されており，それぞれ`Arrows`拡張，`RecursiveDo`拡張を有効にすることで使用可能です．

`Arrow`クラスは，モナドの一般化として導入されました[^ref-generalising-monads-to-arrow]．このクラスには，モナドの`do`構文と同様に，クラスメソッドだけの式に脱糖できる構文が考案され，GHC拡張として実装されています．それが`Arrows`拡張で利用できる`proc`構文です．

[^ref-generalising-monads-to-arrow]: ["Generalising Monads to Arrows"](https://dl.acm.org/citation.cfm?id=347246), John Hughes, in Science of Computer Programming 37, pp. 67–111, May 2000

例えば，`Arrow`クラスのメソッドを使った次のような関数は，

```haskell
doSomething :: Arrow a => a Int Int -> a Int Int -> a Int Int -> a Int Int
doSomething f g h
  =   arr (\x -> (x + 1, x))
  >>> first (f >>> (arr (\y -> 2 * y) >>> g) &&& returnA >>> arr snd)
  >>> arr (\(y, x) -> (x, x + y))
  >>> arr (\(x, z) -> (z, x * z))
  >>> second h
  >>> arr (\(z, t) -> t + z)
```

`proc`構文を使うと，

```haskell
doSomething :: Arrow a => a Int Int -> a Int Int -> a Int Int -> a Int Int
doSomething f g h = proc x -> do
  y <- f -< x + 1
  g -< 2 * y
  let z = x + y
  t <- h -< x * z
  returnA -< t + z
```

というように書けます[^notice-arrow-syntax-reduced]．また，`ArrowLoop`クラスの`loop`メソッドに変換される，`rec`構文も搭載されており次のようなフィードバック制御を相互再帰で行うプログラムを書くことができます．

[^notice-arrow-syntax-reduced]: 一見，この構文は単純な脱糖を行うと脱糖後のプログラムが非常に冗長になるように思えます．しかし，`Arrow`クラスのメソッドに設けられている書き換え規則によって，最終的に妥当な大きさまで脱糖後のプログラムが小さくなってくれます．

```haskell
counter :: ArrowLoop a => (Int -> a Int Int) -> a Bool Int
counter delay = proc reset -> do
  rec output <- returnA -< if reset then 0 else next
      next <- delay 0 -< output + 1
  returnA -< output
```

`proc`構文については[Arrow syntax](https://www.haskell.org/arrows/syntax.html)のページにまとめられている他，[提案論文](http://www.staff.city.ac.uk/~ross/papers/notation.html)にて変換規則を確認することが可能です．

さて，もう1つの`MonadFix`クラスは，モナドを拡張し，再帰的なバインディングを許すようなものです．このクラスを元に，`RecursiveDo`拡張はdo構文をさらに拡張します．具体的には，次のように使用できる`rec`という構文を新たに導入します．

```haskell
doSomething :: [Int]
doSomething = do
  rec x <- [y, y * 10]
      y <- [1, 2]
  pure $ x + y
```

この関数は，次のように`MonadFix`クラスのメソッド`mfix`を使った関数と同等です．

```haskell
doSomething :: [Int]
doSomething = do
  (x, y) <- mfix $ \~(x, y) -> do
    x <- [y, y * 10]
    y <- [1, 2]
    pure (x, y)
  pure $ x + y
```

また，`rec`を省略して書ける`mdo`という構文も提供されます．

```haskell
doSomething :: [Int]
doSomething = mdo
  x <- [y, y * 10]
  y <- [1, 2]
  pure $ x + y
```

`mdo`構文は，それぞれの文と変数の依存関係を解析し，自動的に`rec`ブロックに分けてくれます．後は，その分けられた`rec`文を`mfix`に翻訳することで，通常の`do`構文に翻訳することができます．例えば，

```haskell
mdo
  a <- m
  b <- f a c
  c <- f b a
  z <- h a b
  d <- g d e
  e <- g a z
  pure c
```

という式は，

```haskell
do
  a <- m
  (b, c) <- mfix $ \~(b, c) -> do
    b <- f a c
    c <- f b a
    pure (b, c)
  z <- h a b
  (d, e) <- mfix $ \~(d, e) -> do
    d <- g d e
    e <- g a z
    pure (d, e)
  pure c
```

という式に翻訳されます．`mdo`と`rec`の変換規則は，[提案論文](https://dl.acm.org/citation.cfm?doid=581690.581693)にて確認が可能です．

# 次回予告

今回は，GHC拡張の簡単な紹介と使い方について，それから個人的にデフォルトで有効化している，Preludeの暗黙的なインポートを抑制する拡張，新たな構文を導入する拡張を紹介しました．

次回は，他のデフォルトで有効化している拡張について紹介したいと思います．

# 参考文献

* [GHC 8.4.2 User's Guide](https://downloads.haskell.org/~ghc/8.4.2/docs/html/users_guide/)
    - [GHC 8.4.2 User's Guide - 9. GHC Language Features](https://downloads.haskell.org/~ghc/8.4.2/docs/html/users_guide/glasgow_exts.html#options-language)
    - [GHC 8.4.2 User's Guide - 10. Foreign function interface (FFI)](https://downloads.haskell.org/~ghc/8.4.2/docs/html/users_guide/ffi-chap.html)
* [What I Wish I Knew When Learning Haskell - Language Extensions](http://dev.stephendiehl.com/hask/#language-extensions)
* [Guide to GHC Extensions - Language Standards](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/language-standards)
* [Haskell 2010 Language Report][haskell-lang-report-url]
* [Cabal reference](https://www.haskell.org/cabal/users-guide/cabal-projectindex.html)

[prelude-url]: https://www.stackage.org/haddock/lts-10.8/base-4.10.1.0/Prelude.html
[ghc-user-guide-url]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/
[haskell-lang-report-url]: https://www.haskell.org/onlinereport/haskell2010/
[formal-semantics-of-pattern-matching]: https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-610003.17.3
