---
title: substring-parserで「タイプセーフプリキュア！」を移行した話
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
subHeading: ～タイプセーフプリキュア！を支える技術 その3～
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: September 4, 2018
tags:
...
---

先日私は[プリキュアハッカソン NewStage](https://cure-hack.connpass.com/event/91157/)というちょっと変わったイベントで、「[タイプセーフプリキュア！](https://github.com/igrep/typesafe-precure)」の最近の更新について発表いたしました。  
今回は[その際使用したスライド](http://the.igreque.info/slides/2018-08-18-substring-parser.html)を、ブログ記事として拡大して共有させていただきたいと思います！

# 予告編（はじめにまとめ）

- Haskell界に伝わる伝説のアイテム「パーサーコンビネーター」を応用して、「タイプセーフプリキュア！」の古いソースコードを半自動で変換しました。
- 「パーサーコンビネーター」は正規表現よりいいところたくさんですが、文字列の先頭からのマッチしかできないのがつらいです。
    - [substring-parser](https://gitlab.com/igrep/substring-parser)というライブラリーを書いて、対応しました。
- パーサーコンビネーター最高！ ✌️😆✌️

# これまでのあらすじ

## 「タイプセーフプリキュア！」とは？

[rubicure](https://github.com/sue445/rubicure)や[ACME::PrettyCure](https://github.com/kan/p5-acme-prettycure)のような「[プリキュア実装](https://qiita.com/sue445/items/b41a4f5bdca46f1736c3)」の1つです。  
詳しくはこれから挙げる過去の記事をご覧ください、と言いたいところですが、よくよく見たら「プリキュア実装」が何かを明記してる記事ではないようなので😅、ここで軽く説明しましょう。  
「プリキュア実装」とは一言で言うと「プリキュアやプリキュアに変身する女の子たち、変身時の台詞など諸々のプリキュアの設定をソースコードに収録したライブラリー」です。

例えば、今回取り上げます私の「タイプセーフプリキュア！」は（もちろん）Haskellで書かれたプリキュア実装で、次のように書くことで、キュアアンジュが変身する際の台詞を取得することができます。  
<small>（出力されるリストは、手で整形しています）</small>

```haskell
> import ACME.PreCure
-- キュアアンジュには、薬師寺さあやが「ミライクリスタル・ブルー」を
-- セットした「プリハート」を使うことで変身します。
> transformationSpeech Saaya (PreHeart MiraiCrystalBlue)
[ "ミライクリスタル！"
, "ハートキラっと！"
, "は～ぎゅ～～！"
, "ぎゅ～！"
, "ぎゅ～～！"
, "輝く未来をー、抱きしめて！"
, "みんなを癒す！知恵のプリキュア！キュアアンジュ！"
]
```

GHCiで上記のコードを試す場合は、下記のコードでtypesafe-precureと[unicode-show](https://github.com/haskell-jp/unicode-show)をインストールした上で起動するとよいでしょう。

```bash
$ stack build typesafe-precure unicode-show
$ stack exec ghci -- -interactive-print="Text.Show.Unicode.uprint"
```

その他の機能や、使っているGHCの拡張などについては下記の記事をご覧ください。

- [igreque : Info -> Haskellでプリキュアを作ってみた](http://the.igreque.info/posts/2016/06-type-safe-precure.html)
- [「タイプセーフプリキュア！」を支える技術 - Qiita](https://qiita.com/igrep/items/5496fa405fae00b5a737)

## cure-index.jsonとは？

そんな「タイプセーフプリキュア！」ですが、前述のQiitaの記事の最後で「typesafe-precureは現状非常に冗長で、非実用的な実装になってしまっています」と述べているとおり、ほかのプリキュア実装と異なり、~~実用性を度外視して~~「設定の正しさ」を最優先事項とした結果、変身時の台詞や浄化技（「必殺技」ともしばしば呼ばれます）の台詞を取得するのに、非常に冗長なコードが必要になってしまいました。  
それではせっかくYouTubeやらWikipediaやらBlu-rayやらを見直してせっせと集めた情報が勿体ないので、集めた情報を、コンパイル時にJSONとして出力することにしました。  
そうして生まれたのが[cure-index.json](https://github.com/igrep/typesafe-precure/blob/master/gen/cure-index.json)とそれをプリティープリントした[pretty-cure-index.json](https://github.com/igrep/typesafe-precure/blob/master/gen/pretty-cure-index.json)です。  
将来的には、[かつてrubicureで作ったユナイトプリキュア](http://the.igreque.info/posts/2014-12-25-unite-precure.vim.html)を書き直すのに使用しようかと考えています。

作るに当たって新たに「タイプセーフプリキュア！」のソースコードに仕込んだ仕組みについては、[去年のHaskell Advent Calendarの記事](https://haskell.jp/blog/posts/2017/typesafe-precure2.html)をご覧ください。  
Template HaskellやGHCの`ANN`という機能を濫用することで達成しました。😎

# 今回のプリキュアハッカソンに向けて行ったこと

従来のcure-index.jsonには、最新作である「HUGっと！プリキュア」と、その一つ前の作品である「キラキラ☆プリキュアアラモード」の情報しか収録されていませんでした。  
前述の[去年のHaskell Advent Calendarの記事](https://haskell.jp/blog/posts/2017/typesafe-precure2.html)でも触れましたが、収録のためにはプリキュアの設定の書式を大幅に変更しなければならず、面倒なのでひとまず後回しにしていたのです。

そこで今年のプリキュアハッカソンにて発表するのによいネタだろうと思い、あの手この手を使って、全シリーズをcure-index.jsonに含める対応を行いました[^cure-hack]🎉。

[^cure-hack]: プリキュアハッカソンは「ハッカソン」の名を冠してはいるものの、実態としてはプリキュアの映画を観ながら好き勝手に開発するというゆるい会です。  
また、そもそもそれほど時間もないので、私は当日の3～4週間ほど前から今回の対応を始めておりました。「今回のプリキュアハッカソンに**向けて**行ったこと」なる見出しなのは、そのためです。

## 🔴修正**前**の書式

それでは、具体的にどんな修正を行ったのか紹介しましょう。  
修正前は、プリキュアの設定を収録した各モジュール（`ACME.PreCure.Textbook`以下にあるので、今後は「**各`Textbook`モジュール**」と呼びます）には[👇こんな感じのTypes.hsがたくさん](https://github.com/igrep/typesafe-precure/blob/73948fb4a82baaf4e33900d77326791c7703f786/src/ACME/PreCure/Textbook/MahoGirls/Types.hs#L71)ありました。

```haskell
data CureMiracle = CureMiracle deriving (Eq, Show)

transformedInstance
  [t| CureMiracle |]
  cureName_Miracle
  introducesHerselfAs_Miracle
  variation_Dia
```

上記はキュアミラクルを表す型の定義と、その日本語での名前、変身時の名乗りといったプロフィールを設定しているコードです。  
このほかにも、プリキュアに変身する女の子の設定や、変身の際に必要な変身アイテムなどの型定義がたくさんあります。  
`transformedInstance`で始まる行は、Template Haskellを使った、型クラスのインスタンス宣言です。  
[`transformedInstance`というマクロ](https://github.com/igrep/typesafe-precure/blob/477fc23a018020fe67895e79361520016fd844bf/src/ACME/PreCure/Types/TH.hs#L151-L158)が、[`Transformed`という型クラス](https://github.com/igrep/typesafe-precure/blob/477fc23a018020fe67895e79361520016fd844bf/src/ACME/PreCure/Types.hs#L15-L19)のインスタンスを生成することで、プリキュアを表す型と、日本語での名前、変身時の名乗りを実際に紐付けているのです。  
<small>（実際の日本語での名前はご覧のとおり`cureName_Miracle`といった変数に束縛されております。[Words.hs](https://github.com/igrep/typesafe-precure/blob/73948fb4a82baaf4e33900d77326791c7703f786/src/ACME/PreCure/Textbook/MahoGirls/Words.hs#L18)というファイルから参照しています）</small>

修正前はこのように、あくまでもHaskellのソースコードとして、プリキュアの設定を書いていたため、このままではcure-index.jsonのデータとして扱うのが難しい状態でした。

## 🔵修正**後**の書式

そのため、今回修正した後の各`Textbook`モジュールでは、[👇こんな感じのProfiles.hs](https://github.com/igrep/typesafe-precure/blob/fd5f89797372f616a551e07251c0fcd2ca1531c2/src/ACME/PreCure/Textbook/MahoGirls/Profiles.hs#L20)で、各種の設定を宣言することにしました。

```haskell
transformees :: [Transformee]
transformees =
  [ mkTransformee
      "Cure Miracle"
      ""
      cureName_Miracle
      variation_Dia
      introducesHerselfAs_Miracle
  , ...
  ]
```

`mkTransformee`関数で作っている`Transformee`型の値は、cure-index.jsonの一部として、JSONに変換する中間データです。もちろん`ToJSON`のインスタンスになっております。  
このように新しい各`Textbook`モジュールでは、直接Haskellのソースコードとしてプリキュアの設定を書く代わりに、**一旦JSONに変換する用の中間データを設けることで、cure-index.jsonに収録しやすい状態に**しています。

こうして作られた`Transformee`などの中間データ用の値は、各`Textbook`モジュールのルートに当たるモジュールで、型クラスのインスタンス宣言を行ったり、`ANN`という機能でモジュールに紐付けられます。  
以下は「魔法つかいプリキュア！」のルートに当たるモジュール[`MahoGirls.hs`](https://github.com/igrep/typesafe-precure/blob/477fc23a018020fe67895e79361520016fd844bf/src/ACME/PreCure/Textbook/MahoGirls.hs)からの抜粋です。


```haskell
module ACME.PreCure.Textbook.MahoGirls where

import ACME.PreCure.Textbook.MahoGirls.Profiles

...

{-# ANN module transformees #-}
$(declareTransformees transformees)

...
```

`Profiles.hs`で定義した`transformees`というリストを、`ANN`で`MahoGirls`モジュールに紐付け、`declareTransformees`というTemplate Haskellのマクロで型宣言やインスタンス宣言を生成するのに使っています。  
`ANN`については[前回の「タイプセーフプリキュア！を支える技術」](https://haskell.jp/blog/posts/2017/typesafe-precure2.html)をご覧ください[^types.hs]。

[^types.hs]: 当時は各`Textbook`モジュールの`Types.hs`というファイルで`ANN`や`declareTransformees`などを使っていましたが、現在は「ルートに当たるモジュール」で行うことにしました。ファイル数を減らすのと、exportする識別子を型に絞ることで、`transformeesHugtto`のような、あまりかっこよくない識別子を隠す、というのがその目的です。

修正前との違いにおける要点を繰り返しましょう。修正後の各`Textbook`モジュールでは、

- プリキュアの情報を、
    - cure-index.jsonとして書き出すためのデータ
    - Template Haskellで型や型クラスのインスタンスとして生成するためのデータ
- **両方で扱えるようにするために、専用の型の値として保存**

するようにしています。

## どうやって修正する？

それではここからは、各`Textbook`モジュールの書式を、どうやって前節で説明したような、「修正前」から「修正後」の書式に移行したのか説明します。

当然、手で修正するには大変な量です。  
従来より「タイプセーフプリキュア！」ではTVシリーズ15作品に加えてキュアエコーが出てくる映画もサポートしているため、各`Textbook`モジュールは16作品分存在しています。  
すでに「修正後」の書式に移行済みの「HUGっと！プリキュア」と「キラキラ☆プリキュアアラモード」を除いても、14作品分書き換えないといけません。  
シリーズごとに定義されている型やインスタンス宣言の数にはばらつきがありますが、すべて移行してから数えてみたところ、型の数だけで313個、変身や浄化技のインスタンス宣言だけで211個ありました。  
プリキュアやプリキュアに変身する女の子、変身アイテムだけでなく、それぞれの変種も別の型として定義しているため、実際のプリキュアの数よりも遙かに多いのです😵。  
Vimのマクロなどを駆使すれば決して人間の手でも移行できない規模ではありませんが、そこは「タイプセーフプリキュア！」です。  
始まって以来私がGHCの拡張を始めいろいろな技術を試すための実験場としても機能していたので、ここは是非ちょっと凝ったことをしてぱーっと書き換えてみたいものでしょう😏。  
そこで思いついたのがパーサーコンビネーター、並びに拙作のライブラリー[substring-parser](https://gitlab.com/igrep/substring-parser)だったのです💡！

### パーサーコンビネーターとは

substring-parserの紹介の前に、パーサーコンビネーターについて簡単に紹介しておきましょう。  
<small>（「すでに知ってるよ！」という方はこの節は飛ばした方が良いかと思います）</small>  
パーサーコンビネーターは、例えば正規表現のような、文字列を解析する技術の一つです。  
Haskellの[megaparsec](http://hackage.haskell.org/package/megaparsec)や[attoparsec](http://hackage.haskell.org/package/attoparsec)をはじめ、多くのプログラミング言語にライブラリーとして提供されています。

実装はいろいろありますが、本質的にパーサーコンビネーターは「文字列を受け取って『文字列を解析した結果』と、『残りの文字列』を返す関数」として表現されます。  
加えて、それらを簡単に組み合わせるためのAPIを提供することで、複雑な文字列から複雑なデータ構造を抽出できるようにしてくれます。

実際のパーサーコンビネーターのライブラリーを単純化して例を挙げましょう。  
例えば、通例パーサーコンビネーターのライブラリーは`decimal`という、「10進数の文字列を受け取って、整数を返すパーサー」を提供していることが多いです。

`parse`関数に、解析したい文字列と一緒に渡すことで、「文字列を解析した結果」と、「残りの文字列」を取得することができます。

```haskell
> parse decimal "123abc"
(123, "abc")
```

👆上記の例では「解析したい文字列」として`123abc`を渡したので、パースした結果の整数`123`と、その残りの文字列`"abc"`を返しています。

これだけではつまらないので、ほかのパーサーの例も挙げましょう。  
👇今度は「文字 セミコロン `;` を受け取って、そのまま返すパーサー」です。

```haskell
> parse (char ';') ";aaa"
(';', "aaa")
```

「パースした結果」がセミコロン `;` で、「残りの文字列」が`"aaa"`となっていますね。

それでは以上2つのパーサーを組み合わせて、**10進数の文字列を受け取った後、セミコロンを受け取り、整数を返すパーサー**を作ってみましょう。

```haskell
> decimalAndSemicolon = do
    n <- decimal
    char ';'
    return n

> parse decimalAndSemicolon "123;abc"
(123, "abc") -- 結果にセミコロンが含まれてない点に注意
```

Haskellにおけるパーサーコンビネーターのライブラリーは、パーサーを`Monad`として提供することで、上記のように`do`記法でパーサーを組み合わせることができるようになっています。  
ここでは詳細は割愛しますが、

1. `decimal`で整数をパースしたあと、
1. `char ';'` で文字セミコロン `;`をパース（でも結果は無視）し
1. パースした結果として「`decimal`がパースした整数」`n`を返す

という処理を行っているのがわかるでしょうか？

ちなみに、パーサーコンビネーターに慣れた読者の方なら、いわゆるApplicativeスタイルを使って、次のようにも書けると気づくでしょう。

```haskell
decimalAndSemicolon = decimal <* char ';'
```

これならパースした結果をいちいち変数に束縛する必要もなく、より簡潔に書くことができますね！

パーサーコンビネーターのパワーを実感していただくために、もう一つ例を紹介します。  
`many`という関数にパーサーコンビネーターを渡すと、「受け取ったパーサーコンビネーターで失敗するまで繰り返しパースして、その結果をリストとして返す」パーサーが作れます。  
例えば先ほどの「10進数の文字列を受け取った後、セミコロンを受け取り、整数を返すパーサー」から、「セミコロンが末尾に着けられた整数のリストを返すパーサー」を作ることができます。

```haskell
parse (many decimalAndSemicolon) "12;34;56;"
([12, 34, 56], "")
```

このようにパーサーコンビネーターは、小さなパーサーをどんどん組み合わせることで、複雑な文字列から複雑なデータ構造を取り出すパーサーを、クールに作れるようにしてくれます。

### パーサーコンビネーターが正規表現より良いところ・悪いところ

そんなパーサーコンビネーターについて、正規表現と比べた場合の長所短所を明確にしておきましょう。  
まずはよいところから。

#### 👍パーツとしてパーサーを組み合わせるのが簡単

前節で示したように、複雑なパーサーも、小さなパーサーの組み合わせからコツコツと作れるようになっています。

#### 👍パースした結果を、文字列から複雑なデータ構造に割り当てるのが簡単

さっきの`decimal`は、パースした結果を直接整数(`Int`)として返していたことにお気づきでしょうか？  
正規表現で欲しい文字列からデータ構造を取り出したい際は、通常グルーピング機能を使うことになりますが、必ず一旦文字列として取り出すことになります。  
それに対してパーサーコンビネーターには、取り出した文字列を対象のデータ構造に変換する仕組みが組み込まれています。  
再帰的なパーサーを書いて再帰的なデータ構造に割り当てるのも楽ちんです。

#### 👍パースした結果に基づいて、パーサーの挙動を変えることができる

今回の例にはありませんでしたが、例えばパースして取り出した整数の数だけ、続きの文字列を繰り返しパースする、といったことも簡単にできます。

一方、正規表現と比べて悪いところもあります。

#### 👎記述が冗長

正規表現はいわゆる「外部DSL」、すなわちプログラミング言語から独立した構文で提供されています。  
PerlやRubyなどの構文で言えば、`/.../`の中は別世界ですよね。  
パーサーコンビネーターは、本質的に「文字列を受け取って『文字列を解析した結果』と、『残りの文字列』を返す関数」であるとおり、あくまでプログラミング言語標準の関数<small>（のうち、文字列の解析に特化したもの）</small>として提供されます。「内部DSL」なんて呼ばれることもあります。

そのため、正規表現とは異なり、あくまでもプログラミング言語の構文の中で使えなければならないため、使用できる文字列に限りがあり、必然的に長くなります。  
例えば先ほどの`many`は正規表現で言うところの`*`<small>（0回以上の量指定子）</small>とちょっと似てますが、正規表現の方が3文字も短いですよね。

しかしながら、冗長であることはメリットにもなり得ます👍。  
`*`をはじめ、正規表現の特殊な機能を使うには、専用の記号（メタキャラクター）をその数だけ覚えなければなりません。  
片やパーサーコンビネーターは`many`のような機能も普通の関数として提供されるため、冗長である分分かりやすい名前をつけやすいのです。

#### 👎ユーザーからの入力として直接受け取ることは難しい。

パーサーコンビネーターは先ほども触れた「内部DSL」です。  
つまり、プログラミング言語の普通の関数として使用されるものです。  
したがって、例えば正規表現をエディターの検索機能に利用すると言ったような、「ユーザーからの入力として受け取る」と言ったことは、不可能ではないものの、正規表現に比べれば難しいです。

#### 👎正規表現でいうところの `*` にあたる`many`が、必ず強欲なマッチになる

こちらについてはちょっと難しいので後述します。

#### 👎文字列の先頭からのマッチしかできない

この問題は、パーサーコンビネーターをベター正規表現として使おうと思った場合に、しばしばパーサー作りを面倒くさくします。  
パーサーコンビネーターは、原理上必ず文字列の先頭から解析するよう作られています。  
例えば先ほど紹介したパーサー`decimal`の場合、

```haskell
> parse decimal "abc123"
```

と書いても、`"abc123"`は先頭が「10進数の文字列」ではないので、失敗してしまいます<small>（実際の戻り値はライブラリーによって異なります。試してみましょう！）</small>。

パーサーコンビネーターはそもそもの用途が0からプログラミング言語などのマシンリーダブルな構文を作るところにあるので、妥当と言えば妥当な制限です。  
その場合は必ず、文字列を頭から読んでパースすることになるでしょうから。

とはいえ、これは正規表現で例えるなら、常に先頭に`\A` (あるいは `^`)を付けなければならない、あるいは自動的に付いてしまう、というような制限です。  
正規表現は行の中にある一部の文字列を抽出したり置換したりするのによく使われるので、役に立たないケースがたくさん出てきてしまいます。

パーサーコンビネーターでこの問題に対応するには、マッチさせたい文字列に到達するまで、スキップするための処理を書かないといけません。    
残念ながらこれは、正規表現で言うところの `\A.*(本当にマッチさせたい文字列)` と書けばよい話**ではありません**。  
`\A(マッチさせたくない文字列)*(本当にマッチさせたい文字列)` という書き方をしなければならないのです。  
なぜなら、先ほど触れた「正規表現でいうところの `*` にあたる`many`が強欲なマッチになる」という問題があるためです。  
正規表現で言うところの`\A.*(本当にマッチさせたい文字列)`を書くと、`.*`が「マッチさせたくない文字列」だけでなく「本当にマッチさせたい文字列」までマッチしてしまい、結果肝心の「本当にマッチさせたい文字列」を扱うことができなくなってしまうのです。

### ソースコードの書き換えとsubstring-parser

さて、今回の目的は「『タイプセーフプリキュア！』のソースコードの書式を書き換えることで、全シリーズのプリキュアの情報をcure-index.jsonに収録する」ことでした。  
そのためには、各`Textbook`モジュールのソースコードにおいて**途中**に含まれている、プリキュアを表す型の定義や、型クラスのインスタンス宣言を集める必要があります。  
しかもそれらは、一つの定義が行をまたいでいたりまたいでなかったりするので、よくある行単位で処理するツールを使うのも、なかなか難しいと思います。  
また、抽出したいデータ構造も多様かつそこそこに複雑で、中には再帰的なデータ構造もあります。正規表現を用いてのパースも、かなり困難なことでしょう。  
とはいえパーサーコンビネーターを通常のとおりに使うと、これまでに述べたとおり、「文字列の先頭からしかマッチできない」という制限が、考えることを複雑にします。

こうした状況は今回の問題に限らず、このように、ソースコードの多くの類似箇所を書き換える場面において、しばしば発生するでしょう。  
そこで今回は[^this-time]こうした問題全般に対応するライブラリーとして、[substring-parser](https://gitlab.com/igrep/substring-parser)というライブラリーを作りました。

[^this-time]: 実際には、前職時代に同様の問題に遭遇した際作成しました。今後も必要になったときにちょっとずつ開発していく予定です。

substring-parserを使えば、任意のパーサーコンビネーター[^attoparsec]を**文字列の中間でも**マッチさせることができます。  
残念ながらドキュメントらしいドキュメントが全く書けてない状況ではありますが、一応動きます。  
[Spec.hs](https://gitlab.com/igrep/substring-parser/blob/master/test/Spec.hs)が動作を知る際の参考になるかも知れません。

[^attoparsec]: 一応[`parsers`](http://hackage.haskell.org/package/parsers)パッケージを使って様々なパーサーコンビネーターのライブラリーをサポートするように作りましたが、現状[`attoparsec`](http://hackage.haskell.org/package/attoparsec)でのみテストしています。用途を考えれば多分十分じゃないかと思っています。

#### substring-parserの仕組み

substring-parserはどのようにして、任意のパーサーコンビネーターを文字列の中間でもマッチできるようにしているのでしょう？  
仕組みは単純です。  
引数として受け取ったパーサーを、

1. とりあえず先頭からマッチさせてみる。
1. 失敗したら先頭の一文字をスキップして、次の文字からまたマッチさせてみる。

という手順を繰り返すだけです。 
結果として文字列の先頭にある「マッチさせたくない文字列」をスキップすることができるのです。

⚠️残念ながら決して効率のいい方法ではないので、真面目なパーサーを書くときはおすすめしません！  
あくまでも今回のような、書き捨てだけど、それなりに複雑な文字列を解析する必要がある場合のみ使うべきでしょう。

## 結果、できたもの

ここまで説明したsubstring-parserを駆使することで、私は無事、各`Textbook`モジュールを半自動で古い書式から新しい書式に書き換えることに成功しました。  
<small>（残念ながら古い`Textbook`モジュールには存在しない情報を補ったり、体裁を整えたりする必要があったため、完全に自動で書き換えられたわけではありません）</small>  
[typesafe-precure#25](https://github.com/igrep/typesafe-precure/pull/25)という大きなPull requestに、移行したもののほぼすべてが刻まれています。

なお、上記のPull requestでは消してしまってますが、実際に実行した、移行用スクリプトは[typesafe-precure/app/migrate2cure-index.hs](https://github.com/igrep/typesafe-precure/blob/ed038aa57a4df6b1fcc23fb071253888ebd7d477/app/migrate2cure-index.hs)にあります。  
ご興味のある方はご覧になってみてください。

また、もう少し小さいサンプルとして、プリキュアハッカソンの成果発表でデモをした時点のコミットも載せておきます。  
👇のコマンドを実行すれば、[こちらのコミット時点のパーサー](https://github.com/igrep/typesafe-precure/blob/73948fb4a82baaf4e33900d77326791c7703f786/app/migrate2cure-index.hs#L101-L118)で、[同時点のTypes.hs](https://github.com/igrep/typesafe-precure/blob/73948fb4a82baaf4e33900d77326791c7703f786/src/ACME/PreCure/Textbook/Dokidoki/Types.hs#L19-L23)から、cure-index.jsonで使用する[`Girl`](https://github.com/igrep/typesafe-precure/blob/73948fb4a82baaf4e33900d77326791c7703f786/src/ACME/PreCure/Index/Types.hs#L44-L46)という型の値を取り出すことができます！

```haskell
> git clone https://github.com/igrep/typesafe-precure
> cd typesafe-precure
> git checkout 73948fb4a82baaf4e33900d77326791c7703f786
> stack build :migrate2cure-index
> stack exec migrate2cure-index

... 略 ...

-- src/ACME/PreCure/Textbook/Dokidoki --
Girl {girlId = "\"Mana\"", girlNameEn = "\"Mana\" ++ error \"Need family name!\"", girlNameJa = "girlName"}
Girl {girlId = "\"Rikka\"", girlNameEn = "\"Rikka\" ++ error \"Need family name!\"", girlNameJa = "girlName"}
Girl {girlId = "\"Alice\"", girlNameEn = "\"Alice\" ++ error \"Need family name!\"", girlNameJa = "girlName"}
Girl {girlId = "\"Makoto\"", girlNameEn = "\"Makoto\" ++ error \"Need family name!\"", girlNameJa = "girlName"}
Girl {girlId = "\"Aguri\"", girlNameEn = "\"Aguri\" ++ error \"Need family name!\"", girlNameJa = "girlName"}
```

# その他の似たソリューション

今回は、自前で作ったライブラリーと一から書いたパーサーを組み合わせることで「ソースコードの多くの類似箇所を書き換える」問題に対応しましたが、似たようなことを行うツールはほかにもあります。  
いずれも私はほぼ使ったことがないので詳しい解説はできませんが、軽く紹介しておきます。

## [codemod](https://github.com/facebook/codemod)

Facebook製の一括置換ツールです。指定したディレクトリーのファイル群を、正規表現で一括置換できます。  
ここまで書くと`perl`や`sed`、`awk`などで十分できそうにも聞こえますが、修正前後の状態を色つきで見ながら対話的に修正できるそうです。  
正規表現での単純な修正が気に入らなければ、その場で該当箇所だけをエディタで修正できるとのこと。  
Python 2に依存しているのがちょっとつらいところでしょうか...😨。

## [jscodeshift](https://github.com/facebook/jscodeshift)

同じくFacebookが作った、名前のとおりJavaScriptに特化したソースコードの修正ツールです。  
こちらは正規表現は使用せず、「Transform module」と呼ばれる、JavaScriptのASTを変換するための専用のスクリプトを実行することで修正するそうです。  
様々な状況に特化した「Transform module」を別パッケージとしても提供しているようです。

📝以上の2つについては「[JavaScript疲れに効く！ codemodとJSCodeshiftでリファクタリングが捗る - WPJ](https://www.webprofessional.jp/getting-started-with-codemods/)」も参考にしました。

## [refactorio](https://github.com/SuperpowersCorp/refactorio)

[SuperPowers Corp](https://www.superpowerscorp.com/)という会社が開発中の、lensをはじめとするHaskellのパワーを集大成させた、ソースコードの一括置換ツールです。  
`ByteString -> ByteString`という型のHaskellの関数を渡すことで、指定したディレクトリーのファイルすべてに対して関数を適用し、書き換えます。

加えて、`--haskell`や`--html`、`--javascript`など、各言語に特化したオプションを渡すと、各言語のソースコードを修正するlensベースのmoduleをimportした状態で、関数を作れるようにしてくれます。  
具体的には、例えば`--haskell`オプションを渡すと、[haskell-src-exts](https://hackage.haskell.org/package/haskell-src-exts)と[haskell-src-exts-prisms](https://hackage.haskell.org/package/haskell-src-exts-prisms)パッケージのモジュールをimportすることで、HaskellのASTの各トークンに対応した`Prism`などが使えるようになります。

後は[`biplate`](https://www.stackage.org/haddock/lts-12.8/lens-4.16.1/Data-Data-Lens.html#v:biplate)などlensライブラリーのコンビネーターと組み合わせれば、一気にHaskellのソースコードを編集することができます。
「任意のデータ構造に対するjQuery」とも言われるlensライブラリーのパワーを存分に生かしたツールなのです。

残念なところは、今でも開発中である点と、lensライブラリーに習熟していなければ使いこなせないという点でしょうか。  
よく使う`Lens`型や`Prism`型だけでなく、`Traversal`も使えなければなりません。  
特に[サンプル](https://github.com/SuperpowersCorp/refactorio#haskell-via-haskell-src-exts-and-haskell-src-exts-prisms)で紹介されているような[`biplate`](https://www.stackage.org/haddock/lts-12.8/lens-4.16.1/Data-Data-Lens.html#v:biplate)を使った場合において、指定した`Prism`がマッチしなかった場合、何事もなかったかのようにソースが書き換えられないため、デバッグが面倒なところもつらいです。

# 次のゴール

「タイプセーフプリキュア！」の開発は、これからもプリキュアハッカソンの前後とプリキュアAdvent Calendarの前後を中心に、今後も続ける予定です。  
先にも触れましたが、次回は今回完成させたcure-index.jsonを使用することで、[かつてrubicureで作ったユナイトプリキュア](http://the.igreque.info/posts/2014-12-25-unite-precure.vim.html)を「ユナイトプリキュア」を「ディナイトプリキュア」として書き直すかも知れません。  
ただ、それ以外にももうちょっとHaskellで遊びたいことがあるので、後回しにするかも知れません。  
Vim script、あんまり書きたくないんですよね...😥

# まとめ

- Haskell界に伝わる伝説のアイテム「パーサーコンビネーター」を応用して、「タイプセーフプリキュア！」の古いソースコードを半自動で変換しました。
- 「パーサーコンビネーター」は正規表現よりいいところたくさんですが、文字列の先頭からのマッチしかできないのがつらいです。
    - [substring-parser](https://gitlab.com/igrep/substring-parser)というライブラリーを書いて、対応しました。
- パーサーコンビネーター最高！ ✌️😆✌️

それではこの秋もパーサーコンビネーターでHappy Haskell Hacking!!✌️✌️✌️
