---
title: タイプセーフプリキュア！を支える技術 その2
headingBackgroundImage: ../../img/post-bg.jpg
headingDivClass: post-heading
subHeading: ～定義を自動でまとめる問題にGHCのANNプラグマで挑む～
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: December 24, 2017
tags:
...
---

このエントリーは[Haskell Advent Calendar 2017](https://qiita.com/advent-calendar/2017/haskell) 24日目の記事兼[プリキュア Advent Calendar 2017](https://adventar.org/calendars/2118) 24日目の記事です。  
毎度の手口ですが、二つのAdvent Calendarに同時に投稿しています。

HaskellとプリキュアのAdvent Calendarということで、去年に引き続き「[タイプセーフプリキュア！](https://github.com/igrep/typesafe-precure/)」について、開発する上で見つかった問題と、その解決方法について紹介します [^found-problem]。  
なお、「タイプセーフプリキュア！」そのものの日本語の紹介については、[私の去年のHaskell Advent Calendarの記事](https://qiita.com/igrep/items/5496fa405fae00b5a737)や[同じく去年のプリキュア Advent Calendarの記事](http://the.igreque.info/posts/2016/06-type-safe-precure.html)をご覧ください。

[^found-problem]: 実際には「タイプセーフプリキュアそのものを開発する上で見つかった問題」というよりタイプセーフプリキュアの開発をすることで問題解決の実験をしている、といった方が正しいのは内緒。

# 問題提起

例えば、あなたはたくさんの仲間と、たくさんのサブコマンドがあるCLIアプリを作っていたとします。  
コードの規約上、サブコマンド一つにつき一つのモジュールで、決まった関数<small>（Haskellであれば`[String] -> IO ()`みたいな型の関数でしょうか）</small>を定義するものとします。  
そうした場合、必ずどこかのモジュールで、各モジュールで定義したサブコマンドを表す関数を列挙する必要があるでしょう。  
その場合、次のような問題が生じることがあります。

- サブコマンド（を表す関数）を追加したとき、サブコマンドを列挙しているモジュールに、追加し忘れる。
- 複数の開発者がそれぞれのブランチで、新たに作成したサブコマンドを列挙しているモジュールに追加すると、マージする際にコンフリクトがしばしば発生する。

また、DRY原則を徹底するならば「サブコマンドの名前を、サブコマンド自身の定義と列挙しているモジュールとで繰り返さない」というアイディアに基づき、こうした関数の列挙を避ける、という考え方もあるでしょう。  
そのように作ることで、**モジュールに関わる情報<small>（どのような定義で、どのように使用されるのか）</small>をなるべくモジュールのファイルのみに集約**させることができ、モジュールに関する情報が分散してしまうのを軽減することができます。

つまり、今回実現したいことは、複数のファイルに散らばった特定の関数やデータ型の定義を、自動で一カ所にまとめて再利用する、ということです。  
この記事で何度も使うことになるので「**定義を自動でまとめる問題**」と呼ぶことにします。  
これをGHCの各種機能を利用して、Haskellで実現させる方法を考えましょう。

# ほかの言語での例

こうした処理をHaskell以外のプログラミング言語で行う場合、例えば下記のような機能を使うことになるでしょう。  
参考のために、私がこれまでに出会ったものを紹介します。

## Rubyでの場合 {#typesafe-precure2_case-ruby}

前職時代、私は実際にこの「定義を自動でまとめる問題」に出くわしたのですが、Rubyを使っていたため、下記のように[Module#included](https://docs.ruby-lang.org/ja/2.4.0/method/Module/i/included.html)という、対象のモジュールを`include`<small>（モジュールが提供する機能の継承）</small>したときに呼ばれる、特別なメタプログラミング用のメソッドを使って解決しておりました。

```ruby
module ListedAsSubCommand
  @listed = []

  # このモジュールを include するたびに呼ばれるメソッド。
  # 引数として、include した Class オブジェクト（または Module オブジェクト）を受け取る
  def included klass
    # include した Class オブジェクトをリストに追加して記録する
    @listed.push klass
  end

  class << self
    attr_reader :listed
  end
end


# path/to/commands/foo/sub_command_a.rb
class SubCommandA
  include ListedAsSubCommand

  # SubCommandA の定義 ...
end


# path/to/commands/bar/sub_command_b.rb
class SubCommandB
  include ListedAsSubCommand

  # SubCommandB の定義 ...
end
```

このように書くことで、`ListedAsSubCommand.listed`というプロパティから、`ListedAsSubCommand`を`include`した`Class`オブジェクトのリストが取得できます。  
実際に使用するときは、下記のように、対象のクラスが定義されているファイルを含んだディレクトリーからまとめて`require`した上で、`ListedAsSubCommand.listed`にアクセスする事になるでしょう。

```ruby
# ListedAsSubCommand.includedが実行されるのは対象のクラスが
# 定義されたときなので、この時点では空のリスト。
ListedAsSubCommand.listed #=> []

# Dir.glob メソッドで、指定したディレクトリーから
# 再帰的にファイルを取り出し、require で読み込む。
Dir.glob('path/to/commands/**/*.rb') do|file|
  require file
end

# require されたファイルの中でクラスの定義が実行されるので、
# 定義したクラスがリストに追加される
ListedAsSubCommand.listed #=> [SubCommandA, SubCommandB, ...]
```

## Javaでの場合

Javaで「定義を自動でまとめる問題」を解決する場合も、Rubyと同様に、何らかの形でメタプログラミング用の仕組みを利用することになるかと思います。  
とりわけ、Javaにおいては、この問題の解決に特化しているライブラリーの機能が存在している点が興味深いでしょう。Springの「コンポーネントスキャン」です。

SpringをはじめとするDIフレームワークでは、各クラスにおいて依存するオブジェクト<small>（の、インターフェース）</small>を宣言した際、必ず何らかの形で、「どのインターフェースにどのオブジェクトを紐付けるか」を宣言することになります。いわゆるApplication Contextを書いたXMLであったり、`@Configuration`アノテーションが着いたクラスがそれに当たります。  
結果、モジュール<small>（実際にはJavaなのでクラス）</small>に関する情報、すなわちどのクラスのどのフィールドに、どのオブジェクトを注入するか、といった情報はすべてモジュールのファイルとは独立して管理することになり、DRYではなくなってしまいます。 まさに「定義を自動でまとめる問題」の典型と言えますね。

それに対してSpringの「コンポーネントスキャン」では、下記のように設定することで、「どのインターフェースにどのオブジェクトを紐付けるか」といった情報を、すべて自動で設定してしまうことができます。  
下記はコンポーネントスキャンを`@Configuration`アノテーションが着いたJavaのクラスで設定した場合のサンプルコードです。

```java
@Configuration
@ComponentScan("example.base.package.containing.components")
public class AppConfig {
}
```

`@Configuration`アノテーションを付与したJavaのクラスに、更に`@ComponentScan`というアノテーションを付与すると、Springは、`@ComponentScan`アノテーションの引数として渡した名前空間以下に存在する、すべての`@Component`というアノテーションが着いたクラスのオブジェクトを、自動的にほかの`@Component`が着いたクラスのフィールドとして設定できるようにします[^autowired]。

[^autowired]: もう少し正確に言うと、自動的に設定したいフィールド（あるいはコンストラクターの引数）に`@Autowired`というアノテーションが必要ですが、今回の話では本質的ではないので割愛しています。

```java
@Component
public class SomeComponent {
  // ...
}
```

このようにコンポーネントスキャンを使うことで、`@ComponentScan`されたクラスのオブジェクトは自動で依存するオブジェクトとして紐付けられるようになります。  
従来`foo-context.xml`みたいな名前のファイルに、どのオブジェクトのどのフィールドにどのオブジェクトを紐付けるか、といった情報を一つ一つ書いていたのを、ほとんど書かなくて済むようになりました。

# 解決に必要なもの {#typesafe-precure2_requirement}

さて、私が経験した二つの言語における「定義を自動でまとめる問題」の解決方法を見てきたところで、この問題を解決するのに共通して必要なことを列挙しましょう。

**(1) 対象となる「まとめたい定義（モジュールや関数、型など）」が書かれているファイルが、どのディレクトリー以下にあるか設定する**

「定義を自動でまとめる問題」に取り組むに当たり、最低限必要となるのが、この設定です。  
まさかファイルシステムにあるすべてのソースコードから「まとめたい定義」を探すわけにも行きませんし、プロジェクトのディレクトリーすべてを処理するのも、柔軟性に欠けた解決方法でしょう。そこで通例「定義を自動でまとめる問題」に対応する際は、「まとめたい定義（モジュールや関数、型など）」が書かれているファイルがどのディレクトリー以下にあるか、を何らかの形で書くことになります。

前述のRubyによる例の場合、この情報は下記の`Dir.glob`メソッドに渡した引数に当たります。  
`'path/to/commands/**/*.rb'` という文字列のうち、 `path/to/commands/` の部分ですね。

```ruby
Dir.glob('path/to/commands/**/*.rb') do|file|
  require file
end
```

JavaにおけるSpringのコンポーネントスキャンの場合、`@ComponentScan`アノテーションに渡した引数が該当します。  
厳密には、`@ComponentScan`アノテーションに渡す引数はディレクトリーのパスではなく`Java`のパッケージの名前ですが、Javaではパッケージはクラスパス以下のディレクトリーと一対一で対応するよう作る必要があるので、事実上ディレクトリーのパスを渡していると言えるでしょう。

```java
@Configuration
@ComponentScan("example.base.package.containing.components")
public class AppConfig {
}
```

**(2) 「まとめたい定義（モジュールや関数、型など）」が書かれたファイルに、なんらかの印をつける**

「定義を自動でまとめる問題」では、「どの定義を自動でまとめるか」さえ指定できればよいので、理屈の上では前述の「(1) 対象となる『まとめたい定義（モジュールや関数、型など）』が書かれているファイルが、どのディレクトリー以下にあるか設定する」さえできれば、後はディレクトリー以下のファイルをすべて自動でまとめられるはずです。
しかし、それだけでは次の問題が生じてしまう恐れがあります。

1. 「自動でまとめられるファイル」がどのように使用されるか理解しにくくなる。
    - 「自動でまとめられるファイル」に書かれた定義は、多くの場合、明確に使用される箇所で言及されなくなってしまいます。結果、そのファイルを読んだだけでは、書かれている定義がどこでどう使われているのか、そもそも本当に使われているのかどうかすら分からなくってしまいます。プロジェクトに新しく参加する人は、相応の学習が必要になってしまうでしょう。
1. 細かい例外を設定しにくい。
    - 「まとめたい定義が書かれたファイル」を含むディレクトリーの中に、まとめる対象としたくないファイルを作る、ということがやりにくくなってしまいます。
    - 例えばサブコマンドの例で言えば、`Commands`というディレクトリー以下に複数のサブコマンド（まとめられる対象）を置いたとき、各サブコマンドで共有されるユーティリティー関数も`Commands`ディレクトリー以下に置きたくなるかも知れません。もちろん状況に応じてほかのディレクトリーに置く手段も検討すべきですが、そうしたユーティリティー関数の入ったファイルは自動でまとめて欲しくないでしょう。

そうした問題を軽減するために、「定義を自動でまとめる問題」に対応する際は、必ず「『まとめたい定義（モジュールや関数、型など）』が書かれたファイルに、なんらかの印をつける」ことを検討した方がいいと思います。

前述のRubyによる例で言えば、これは`include ListedAsSubCommand`という、`included`メソッドを実装した`ListedAsSubCommand`モジュールを`include`することが該当します。  
JavaのSpringのコンポーネントスキャンの場合、まさしく`@Component`アノテーションがそれに当たるでしょう。

これらの印が着いたファイルを読む場合、この「印」を手がかりにして、コードベースを検索したり定義ジャンプしたり、Springの場合はインターネットを検索したりすることで、「印」の役割を知り、そのファイルがどう使われるのか調べることができるのです。

# 注意点 {#typesafe-precure2_warnings}

いよいよ次の節で「定義を自動でまとめる問題」をHaskellで解決した例を紹介いたしますが、その前にこの問題を解決することによって生じる、副作用について強調しておきましょう。
私の観測範囲内でですが、今までこの問題に対応した例を見たことがないのは、そうした副作用による悪影響が大きいと感じている人が多数派だからなのかも知れません。

それは、前節でも触れましたが、「『自動でまとめられるファイル』がどのように使用されるか理解しにくくなる」ということです。  
この問題は、確かに「『まとめたい定義（モジュールや関数、型など）』が書かれたファイルに、なんらかの印をつける」ことである程度緩和可能な問題ではありますが、それでも強く意識するべきでしょう。  
「自動でまとめられるファイル」を初めて読んだ人が、`include ListedAsSubCommand`や`@Component`という印に気づければよいのですが、そうでない場合、使用箇所を求めてコードベースをさまようことになってしまいます。  
事前に「印」の存在を知らせておくに越したことはありません。

それから、「『まとめたい定義（モジュールや関数、型など）』が書かれたファイルに、なんらかの印をつける」ことを選択した場合、「まとめたい定義が書かれたファイル」を新しく追加したいとき、ファイルにその「印」を書き忘れてしまうことがある点も、覚えておくべきでしょう。  
当初この「定義を自動でまとめる問題」を提起した際、自動でまとめなかった場合のデメリットしてあげた、

> - サブコマンド（を表す関数）を追加したとき、サブコマンドを列挙しているモジュールに、追加し忘れる。

という問題と本質的に同じです。  
自動でまとめずに手で定義を列挙した場合と比べて、編集するファイルが少ない分、忘れる可能性は低いかもしれません。  
ひな形に「印」を含めれば、さらに忘れる確率を下げることができるでしょう。手で一つのファイルに定義を列挙していた場合、そうした工夫はできません。  
ですが、いずれにしても忘れてしまうリスクがあることは変わらないでしょう。

以上の通り、結局のところ、「定義を自動でまとめる」よう設定するか、単純にまとめたい定義を手で列挙するかどうかは、そうしたトレードオフを考慮しつつ落ち着いて考えるのを推奨します。  
これから紹介する方法を採用する際も、ここであげた注意点については忘れないでください。

# Haskellでの解決事例 - 「タイプセーフプリキュア！」における`cure-index.json`の実装

「[タイプセーフプリキュア！](https://github.com/igrep/typesafe-precure/)」<small>（パッケージとしての名前は[typesafe-precure](https://hackage.haskell.org/package/typesafe-precure)なので、以下「typesafe-precure」と呼びます）</small>では、最近の更新により、コンパイル時に「[cure-index.json](https://github.com/igrep/typesafe-precure/blob/master/gen/cure-index.json)」と、「[pretty-cure-index.json](https://github.com/igrep/typesafe-precure/blob/master/gen/pretty-cure-index.json)」いうファイルを生成するようになりました。  
次のような内容のファイルです。

```json
{
    "specialItems": [
        {
            "nameEn": "Sweets Pact",
            "attachments": [
                "Animal Sweets"
            ],
            "nameJa": "スイーツパクト",
            "id": "SweetsPact"
        },
        ...
    ],
    "transformees": [
        {
            "nameEn": "Cure Whip",
            "variationEn": "",
            "nameJa": "キュアホイップ",
            "variationJa": "",
            "id": "CureWhip",
            "introducesHerselfAs": "ショートケーキ！元気と笑顔を！レッツ・ラ・まぜまぜ！キュアホイップ！できあがり！"
        },
        ...
    ],
    "girls": [
        {
            "nameEn": "Ichika Usami",
            "nameJa": "宇佐美 いちか",
            "id": "Ichika"
        },
        ...
    ],
    ...
}
```

これは、変身アイテムからプリキュア、プリキュアに変身する前の女の子、それから浄化技や変身時の台詞まで、typesafe-precureで定義されているあらゆる情報をまとめたJSONです。  
まさしく、プリキュアの定義を自動でまとめた「インデックス」となっております [^index]。  
ただし、残念ながら現時点では「キラキラ☆プリキュアアラモード」に収録されたプリキュアの情報しか、`cure-index.json`には記録されていません<small>（理由は後で説明します）</small>。

[^index]: もちろん、数年前流行ったあのライトノベルのパロディーではありません。

名前の通り、pretty-`cure-index.json`には`cure-index.json`をプリティープリントしたJSONが記録されています。  
下記のように`curl`して確かめてみましょう。  

```bash
$ curl -sL https://github.com/igrep/typesafe-precure/raw/master/gen/cure-index.json
{"girls":[{"id":"Ichika","nameEn":"Ichika Usami","nameJa":"宇佐美 いちか"},{"id":"Himari","nameEn":"Himari Arisugawa","nameJa":"有栖川 ひまり"}
...
$ curl -sL https://github.com/igrep/typesafe-precure/raw/master/gen/pretty-cure-index.json
{
    "specialItems": [
        {
            "nameEn": "Sweets Pact",
...
```

さて、この`cure-index.json`、繰り返しになりますが、typesafe-precureで定義されている、すべてのプリキュアの情報をまとめたJSONとなっております。  
ライブラリーとしてのtypesafe-precureでは、これらの情報は一つ一つがHaskellの型として定義[^detail-typesafe-precure]されており、`cure-index.json`は、それらの情報をコンパイル時に「自動でまとめる」ことで作成されます。決して、JSONからHaskellの型を作っているわけではありません。  
詳細は冒頭にも挙げましたが、[私の去年のHaskell Advent Calendarの記事](https://qiita.com/igrep/items/5496fa405fae00b5a737)や[同年のプリキュア Advent Calendarの記事](http://the.igreque.info/posts/2016/06-type-safe-precure.html)をご覧ください。  
ここではそれを実現するために使用した、Haskellで「定義を自動でまとめる」方法を紹介しましょう。

## 使用したGHCについて

...と、その前に、今回typesafe-precureのビルドに使用したGHCのバージョンを述べておきましょう。

typesafe-precureは現在(ver. 0.5.0.1)の時点において、通常GHC 8.0.2でビルドされています。  
特にCIでの確認はしていませんが、GHC 7.10でもビルドできるはずです。  
従って、使用しているtemplate-haskellパッケージは[2.10.0.0](https://hackage.haskell.org/package/template-haskell-2.10.0.0)から[2.11.1.0](https://hackage.haskell.org/package/template-haskell-2.11.1.0)となっています。

この記事で紹介する機能は、GHC<small>（と、GHCに標準添付されるtemplate-haskellパッケージ）</small>のバージョンによって、大きく変わる場合があります。  
今回は「できない」としたことも、将来のGHCではできるようになっている（あるいは運悪くその逆もある）かもしれません。  
あらかじめご了承ください。

なお、各バージョンのGHCに標準添付されているパッケージのバージョンについては、[Commentary/Libraries/VersionHistory – GHC](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/VersionHistory)をご覧ください。

## ANNで「まとめたい型」が書かれたモジュールに「印」を着ける

まず、「『まとめたい定義（モジュールや関数、型など）』が書かれたファイルに、なんらかの印をつける」方法を考えましょう。  
実はHaskell(GHC)にもアノテーションがあります<small>（Javaのアノテーションと使い勝手が異なりますが）</small>。  
`ANN`という[GHCのプラグマ](https://www.haskell.org/onlinereport/haskell2010/haskellch12.html#x19-18800012)（`{-# ... #-}` という形式で表される、特別なコメント）を使用すると、下記のように、モジュールや型、名前が付いた値に対して、アノテーションを加えることができます<small>（例は[アンッ!!!アンッ!!!! - Qiita](https://qiita.com/philopon/items/85210cc8f23ae04ba6ec)から拝借しました）</small>。

```haskell
module Foo where
{-# ANN module ("annotation" :: String) #-} -- モジュールに対する注釈。importの前には書けないっぽい。不便……

data Foo = Foo
{-# ANN type Foo (2 :: Int) #-} -- 型に対する注釈
{-# ANN type Foo (5 :: Int) #-} -- 注釈を同じ/違う型で複数個付ける事も出来る
{-# ANN type Foo (2.4 :: Double) #-}

foo :: Foo
foo = Foo
{-# ANN foo (3 + 2 * 6 :: Int) #-} -- 値に対する注釈。注釈の中で計算する事も可能
```

上記の通り、GHCの`ANN`は、Javaのアノテーションと異なり、アノテーション専用のインターフェースを作って引数を補足情報として渡す、というような形式ではありません<small>（そもそもHaskellにはインターフェースなんてありませんしね）</small>。  
`Data`型クラスのインスタンスである型の値であれば、なんでもアノテーションとして設定できます。

その`Data`型クラスのインスタンスですが、`base`パッケージに含まれている多くの型に加え、`DeriveDataTypeable`というGHCの言語拡張を使えば、オリジナルの型も簡単にそのインスタンスにすることができます。

```haskell
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data

data SomeOriginalType =
  SomeOriginalValue deriving Data
```

この、`Data`型クラスを使えば、実行時に型の構造を取得したりすることができます。  
とはいえ、ここでは単純に`{-# LANGUAGE DeriveDataTypeable #-}`と`deriving Data`を「おまじない」として使うだけで差し支えありません。  
詳しく知りたい方は[「What I Wish I Knew When Learning Haskell 日本語訳」の「ジェネリクス」の章](https://github.com/shiatsumat/wiwinwlh-jp/wiki/%E3%82%B8%E3%82%A7%E3%83%8D%E3%83%AA%E3%82%AF%E3%82%B9)をご覧ください。

さてtypesafe-precureでは、この`Data`型クラスと`ANN`プラグマを利用した次のようなアプローチで、各モジュールに対し、プリキュアやプリキュアに関する情報を「印」として付与しました。

1. [`ACME.PreCure.Index.Types`](https://github.com/igrep/typesafe-precure/blob/f6701b3b4a86fda3a9e82a6f0c06a87c4a56362e/src/ACME/PreCure/Index/Types.hs)というモジュールに、型の名前やインスタンスの定義を自動生成したり、それをJSONに変換したりするのに使う、中間データのための型を作る。

    ```haskell
    data Girl =
      Girl { girlId :: String, girlNameEn :: String, girlNameJa :: String }
        deriving (Eq, Show, Data)
    ```
    - この、各種中間データ用の型を`Data`型クラスのインスタンスとすることで、「まとめたい定義」が含まれたモジュールに、その中間データ用の値を`ANN`プラグマで付与できるようにする。
1. 名前が`ACME.PreCure.Textbook.*.Profiles`という形式のモジュール[^textbook]<small>（[「キラキラ☆プリキュアアラモード」での例](https://github.com/igrep/typesafe-precure/blob/f6701b3b4a86fda3a9e82a6f0c06a87c4a56362e/src/ACME/PreCure/Textbook/KirakiraALaMode/Profiles.hs)）</small>で、中間データの値<small>（つまり各プリキュアや変身アイテムなどについての情報）</small>を定義する。

    ```haskell
    girls :: [Girl]
    girls =
      [ mkGirl "Ichika Usami" "宇佐美 いちか"
      , mkGirl "Himari Arisugawa" "有栖川 ひまり"
      , mkGirl "Aoi Tategami" "立神 あおい"
      , mkGirl "Yukari Kotozume" "琴爪 ゆかり"
      , mkGirl "Akira Kenjo" "剣城 あきら"
      , mkGirl "Ciel Kirahoshi" "キラ星 シエル"
      ]
    ```
<small>1. `ACME.PreCure.Textbook.*.Profiles`で定義した中間データを、`ACME.PreCure.Textbook.KirakiraALaMode.Types`という形式のモジュールに対して`ANN`プラグマで付与する（同じく[「キラキラ☆プリキュアアラモード」での例](https://github.com/igrep/typesafe-precure/blob/f6701b3b4a86fda3a9e82a6f0c06a87c4a56362e/src/ACME/PreCure/Textbook/KirakiraALaMode/Types.hs)）</small>。

    ```haskell
    module ACME.PreCure.Textbook.KirakiraALaMode.Types where

    import ACME.PreCure.Textbook.KirakiraALaMode.Profiles

    {-# ANN module girls #-}
    ```


[^textbook]: 誰にも聞かれてはいませんが勝手にお話ししますと、`ACME.PreCure.Textbook`という名前は、[プリキュアの教科書](https://dic.pixiv.net/a/%E3%83%97%E3%83%AA%E3%82%AD%E3%83%A5%E3%82%A2%E6%95%99%E7%A7%91%E6%9B%B8)から来ています。

### Stage Restrictionを避けるためにモジュールを分ける

先の手順で引用したコードをご覧になった方は、こんなことを疑問に思ったかも知れません。  
中間データの値を定義するモジュールと、`ANN`で中間データの値を付与するモジュールとを分ける必要があるのか、と。  
上記の例で言えば、一つのモジュール(`ACME.PreCure.Textbook.KirakiraALaMode.Types`)で`girls`を定義しつつ`ANN`で付与すればよいのではないか、ということです。  
あるいは`girls`という名前をつけずに、

```haskell
{-# ANN module
    [ mkGirl "Ichika Usami" "宇佐美 いちか"
    , mkGirl "Himari Arisugawa" "有栖川 ひまり"
    , mkGirl "Aoi Tategami" "立神 あおい"
    , mkGirl "Yukari Kotozume" "琴爪 ゆかり"
    , mkGirl "Akira Kenjo" "剣城 あきら"
    , mkGirl "Ciel Kirahoshi" "キラ星 シエル"
    ]
#-}
-- 注: このコードは試していないので文法が合っているか自信がないです。
```

というような書き方はできないのか、ということです。

中間データの値を`ANN`で使うだけならそれで問題ないのですが、typesafe-precureの場合、中間データの値からプリキュアや変身アイテムを表す型と、その型クラスのインスタンスを定義する必要があります。  
なので、先ほどのコード例にあった`ACME.PreCure.Textbook.KirakiraALaMode.Profiles`というモジュールでは、実際には`{-# ANN module girls #-}`の行の後に、`girls`から、プリキュアに変身する女の子（を表す型）や、それに対して型クラスのインスタンスを宣言するTemplate Haskellのコードが続いています。  
下記の`$(declareGirls girls)`という行がそれです。

```haskell
{-# LANGUAGE TemplateHaskell #-}
module ACME.PreCure.Textbook.KirakiraALaMode.Types where

import ACME.PreCure.Textbook.KirakiraALaMode.Profiles

{-# ANN module girls #-}
$(declareGirls girls)
```

詳細は冒頭でも挙げた[私の去年のHaskell Advent Calendarの記事](https://qiita.com/igrep/items/5496fa405fae00b5a737)などをご覧いただきたいのですが、typesafe-precureでは、それぞれのプリキュアや、プリキュアに変身する女の子、変身に必要なアイテムなどを、すべて**個別の型**として定義しています。  
そのため、中間データの値はJSONとしてまとめるだけでなく、個別の型として定義する必要もあったのです。  
その結果、中間データの値は必ず名前をつけて使い回さないといけなくなるのです。

そして、`ANN`やTemplate Haskellにおいて値に名前をつけて使い回す場合、「Stage Restriction」というやっかいな制限が顔を出してきます。  
これは、「`ANN`で値を付与する式や、トップレベルの宣言などを生成するTemplate Haskellのコードでは、**ほかのモジュールから`import`された名前しか**参照できない」という制限です<small>（詳しくは「[できる！Template Haskell (完)](http://haskell.g.hatena.ne.jp/mr_konn/20111218/1324220725)」をご覧ください）</small>。  
これがあるために、中間データの値を含めた名前（上記のコードの場合`girl`）は、`ANN`やTemplate Haskellで参照するモジュールとは一旦別のモジュールとして定義して、`import`して再利用するしかありません。

本来、「定義を自動でまとめる問題」に対応する目的の中には「モジュールに関わる情報（どのような定義で、どのように使用されるのか）をなるべくモジュールのファイルのみに集約させる」というものがありましたが、外部のファイルに書くボイラープレートが増えてしまい、この観点ではイマイチな実装になってしまいました。  
この点については、後の節でよりよい方法を検討しましょう。

## autoexporterで「まとめたい型」が書かれているモジュールが、どのディレクトリー以下にあるか設定する

前節までで紹介した方法により、`ANN`プラグマを使うことでプリキュアの情報が書かれたモジュールに、プリキュアの情報を「自動でまとめる」ための「印」を着けることができました。  
続いて、`ANN`プラグマで「印」を着けたモジュールがどこにあるかを指定して、GHCに自動で回収させる方法を述べましょう。
「[解決に必要なもの](#typesafe-precure2_requirement)」の節で説明した、「対象となる『まとめたい定義（モジュールや関数、型など）』が書かれているファイルが、どのディレクトリー以下にあるか設定する」部分に当たります。

次の節で説明しますが、`ANN`プラグマで付与した情報は、「[アンッ!!!アンッ!!!!」でも説明されている](https://qiita.com/philopon/items/85210cc8f23ae04ba6ec#template-haskell%E3%81%8B%E3%82%89)とおり`reifyAnnotations`というTemplate Haskellの関数を使えば取得することができますが、該当のモジュールを何らかの方法で集めなくてはなりません。  
私が調べた限り、少なくともTemplate Haskellを使う限りは、`import`しているモジュールから収集する方法しか見つかりませんでした。  
[Template Haskellのライブラリーのドキュメント](https://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH.html)では、`reifyAnnotations`するのに必要な、`Module`型の値を取得する方法として、`thisModule`を使ってTemplate Haskellのコードを実行しているモジュールから取得するか、`reifyModule`関数を使って`thisModule`から`thisModule`が`import`しているモジュールから取得するしか紹介されていないためです。

しかし、現状のGHCではTemplate Haskellをもってしても、指定したディレクトリー以下のモジュールを自動で`import`するということはできません。  
あまりユーザーに自由を与えてしまうと、却って混乱が生じる恐れがあるので敢えて実装していないのでしょう。  
とは言え、だからといって「印」を着けたモジュールを一つずつ手で`import`して列挙してしまっては、「定義を自動でまとめる問題」を解決できたとは言えなくなってしまいます。  
そこで、今回は実践でもよく使われる、さらなる「裏技」を用いることにしました。  
本節の見出しでネタバレしてしまっていますが、[`autoexporter`](https://hackage.haskell.org/package/autoexporter)というプログラムと、[GHCのカスタムプリプロセッサーのためのオプション](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#ghc-flag--F)を使います。

autoexporterは、ドキュメントに書いてあるとおり、GHCのカスタムプリプロセッサーのためのオプション(`-F -pgmF`)、さらには[`OPTIONS_GHC`プラグマ](https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/pragmas.html#options-pragma)組み合わせて、次のように使うことを想定して作られています。  
以下は、[typesafe-precureの`ACME/PreCure/Textbook.hs`というファイル](https://github.com/igrep/typesafe-precure/blob/f6701b3b4a86fda3a9e82a6f0c06a87c4a56362e/src/ACME/PreCure/Textbook.hs)からの抜粋です。

```haskell
{-# OPTIONS_GHC -F -pgmF autoexporter #-}
```

と、いっても1行だけですが😅

一つずつ解説しましょう。  
まず`OPTIONS_GHC`プラグマですが、文字通りこれは`ghc`コマンドに渡すオプションを、ファイル単位で指定するためのものです<small>（もちろんすべてのオプションをファイル単位で指定できるわけではありません）</small>。  
つまり、上記の場合`-F -pgmF autoexporter`というオプションが、`ACME/PreCure/Textbook.hs`というファイルでのみ有効になります。

続いて`-F`オプションですが、これは「カスタムプリプロセッサー」という機能を有効にするためのものです。  
これを有効にすると、有効にしたファイルを、続く`-pgmF`オプションで指定したプログラムで変換するようになります。  
具体的には、`-pgmF`オプションで指定したプログラムに、

1. 変換前のファイル名、
1. 変換前のソースコードを含むファイルの名前<small>（恐らく、一時ディレクトリーにコピーした、変換前のファイル名とは異なる名前と思われます）</small>、
1. 変換後のソースコードを書き込むファイル名<small>（これも一時ディレクトリーにあるファイル名なのでしょう）</small>、

という3つのコマンドライン引数を渡して、`-pgmF`オプションで指定したプログラムを実行します。  
`-pgmF`で指定したプログラムが、3つめの引数として渡した名前のファイルに変換後のソースコードを書き込むことで、`-F`を有効にしたファイルを、変換後のソースコードでそっくりそのまま差し替えます。  
結果、`-pgmF`オプションで指定したプログラムは、自由に任意のHaskellのソースを生成できるようになります。まさにソースコードの自動生成にぴったりな機能と言えるでしょう。

ちなみにこの機能、[`hspec-discover`](https://hackage.haskell.org/package/hspec-discover)などのパッケージでも使用されています。テストコードを複数のファイルに分けて書く場合はほぼ必ず使われるものなので、みなさんも「おまじない」として使用したことがあるでしょう<small>（`-F -pgmF`なんて文字列、ググラビリティーも低いですしね。）</small>。  
そういえばこれもテストコードの「定義を自動でまとめる問題」を解決したものでしたね！

話がそれましたが、`autoexporter`はこのカスタムプリプロセッサーを利用することで、次のようなソースコードを自動生成します。  
`autoexporter`のドキュメントにも同じことが書かれていますが、ここでも`ACME/PreCure/Textbook.hs`を例に説明しましょう。

```haskell
module ACME.PreCure.Textbook
  ( module ACME.PreCure.Textbook.First
  , module ACME.PreCure.Textbook.MaxHeart
  ...
  , module ACME.PreCure.Textbook.KirakiraALaMode
  ) where

import ACME.PreCure.Textbook.First
import ACME.PreCure.Textbook.MaxHeart
...
import ACME.PreCure.Textbook.KirakiraALaMode
```

そう、（プリキュアが好きで）賢明なHaskellerのみなさんならお気づきでしょう。[typesafe-precureの`ACME/PreCure/Textbook/`ディレクトリー](https://github.com/igrep/typesafe-precure/tree/f6701b3b4a86fda3a9e82a6f0c06a87c4a56362e/src/ACME/PreCure/Textbook)に含まれている、（プリキュアの各シリーズを表す）すべてのモジュールを`import`して、再エクスポートしているのです！

つまり、`autoexporter`はこのような、「責務を分割するためにモジュールを細かく分けたい、でもユーザーには一つのモジュールを`import`しただけで使えるようにしたい」というライブラリー開発者のニーズに応えるため、よく行われているモジュールの書き方を自動で行うための便利コマンドなのです。

紹介が長くなりましたが、typesafe-precureではこの`autoexporter`を次のように使うことで、「まとめたい型（プリキュアや変身アイテムなどの情報）」が書かれているモジュールを集めています。

1. 前述の`ACME.PreCure.Textbook`モジュールで`autoexporter`を使うことで、`ACME.PreCure.Textbook`以下にある、「まとめたい型（プリキュアや変身アイテムなどの情報）」が書かれているモジュールをすべて自動的に再エクスポートする。
1. [`ACME.PreCure.Index`](https://github.com/igrep/typesafe-precure/blob/f6701b3b4a86fda3a9e82a6f0c06a87c4a56362e/src/ACME/PreCure/Index.hs)モジュールが`ACME.PreCure.Textbook`モジュールを`import`することで、実際に`cure-index.json`などの書き出しを行う`ACME.PreCure.Index`モジュールが、`ACME.PreCure.Textbook`が再エクスポートしたすべてのモジュールを利用できるようになる。

実際のところ`OPTIONS_GHC -F`をもっとうまく使えば、`ACME.PreCure.Textbook`以下にあるモジュールを自動ですべて`import`するモジュールと、それを利用して`cure-index.json`などの書き出しを行うモジュールを、分けずに一つのモジュールで済ますこともできたでしょう。  
今回は敢えて`autoexporter`を再利用することで、`ACME.PreCure.Textbook`以下にあるモジュールをすべて回収する処理を書かずに任せることにしました。  
この件については後ほど再検討しましょう。

## `ANN`プラグマで付与した定義情報から、JSONを書き出す

いよいよ、`autoexporter`を駆使して集めたモジュールから、`ANN`で付与したプリキュアの情報を取り出し、JSONに変換して書き出しましょう。  
詳細は[`ACME.PreCure.Index`](https://github.com/igrep/typesafe-precure/blob/f6701b3b4a86fda3a9e82a6f0c06a87c4a56362e/src/ACME/PreCure/Index.hs)モジュールや、[`ACME.PreCure.Index.Lib`](https://github.com/igrep/typesafe-precure/blob/f6701b3b4a86fda3a9e82a6f0c06a87c4a56362e/src/ACME/PreCure/Index/Lib.hs)モジュールのソースコードをご覧いただきたいのですが、ここでは簡単にアルゴリズムを解説します。

1. 「現在のモジュール（`ACME.PreCure.Index`）」を取得する。
1. 「現在のモジュール」が`import`しているモジュールから、`ACME.PreCure.Textbook`モジュールを見つけて、取り出す（[具体的には38行目から39行目](https://github.com/igrep/typesafe-precure/blob/f6701b3b4a86fda3a9e82a6f0c06a87c4a56362e/src/ACME/PreCure/Index.hs#L38-L39)）。
1. 取得した`ACME.PreCure.Textbook`モジュールが`import`している、プリキュアの情報を集めたモジュール（`ANN`プラグマでプリキュアの情報を付与したモジュール）をすべて取り出す（[具体的には42行目から45行目](https://github.com/igrep/typesafe-precure/blob/f6701b3b4a86fda3a9e82a6f0c06a87c4a56362e/src/ACME/PreCure/Index.hs#L42-L45)）。
1. 「プリキュアの情報を集めたモジュール」すべてから、`ANN`プラグマで付与されているプリキュアや変身アイテムなどの情報を集めて、種類ごとに一つのリストとしてまとめる（[具体的には48行目から60行目](https://github.com/igrep/typesafe-precure/blob/f6701b3b4a86fda3a9e82a6f0c06a87c4a56362e/src/ACME/PreCure/Index.hs#L48-L60)）。
1. 収集してできた`Index`という型の値を、それぞれJSONに変換して書き込む（[具体的には48行目から60行目](https://github.com/igrep/typesafe-precure/blob/f6701b3b4a86fda3a9e82a6f0c06a87c4a56362e/src/ACME/PreCure/Index.hs#L61-L62)）。

上記のアルゴリズムにおいても、Template Haskellの「Stage Restriction」と戦わなければならないということは注記しておきましょう。  
つまり、`ACME.PreCure.Index`におけるTemplate Haskellのコードで繰り返し使う便利な関数は、`ACME.PreCure.Index`とは別のモジュールで定義して、`import`して使わなければならないのです。  
`ACME.PreCure.Index.Lib`モジュールは、その制限を回避するためのモジュールです。

ともあれこうして、typesafe-precureでは`ACME.PreCure.Index`モジュールをコンパイルする度に、各モジュールに定義されたすべてのプリキュアに関する情報を集めて、[genディレクトリー](https://github.com/igrep/typesafe-precure/tree/f6701b3b4a86fda3a9e82a6f0c06a87c4a56362e/gen)にある`cure-index.json`や`pretty-cure-index.json`というファイルに書き出すことができました。  
「定義を自動でまとめる問題」、これにて一件落着です！🎉  
なお、自動生成されるファイルをGitで管理することはなるべく避けた方がよいことですが、`cure-index.json`の配布を簡単に行うため方策として用いることにしています。

# うまくいかなかった方法 （+ 来年の「タイプセーフプリキュア！」についてちょっとだけ）

typesafe-precureにおける「定義を自動でまとめる問題」の解決方法はここまで述べたとおりですが、今後同じような問題に対応したくなったときのために、最初に思いついたけどうまくいかなかった方法や、後で思いついた別の解決方法をこの先の二つの節ででまとめておきます。  
私や読者のみなさんがお仕事など、より重要なプロジェクトでこれらのアイディアを活かすことができれば幸いです。

## 型クラスのインスタンスから

当初（実は今も大部分は）、typesafe-precureには、[`ACME.PreCure.Textbook.KirakiraALaMode.Profiles`](https://github.com/igrep/typesafe-precure/blob/f6701b3b4a86fda3a9e82a6f0c06a87c4a56362e/src/ACME/PreCure/Textbook/KirakiraALaMode/Profiles.hs)で定義しているような中間データはなく、各プリキュア（や、変身アイテムなど諸々）に対しては、直接型を宣言したり型クラスのインスタンスを実装したりしていました。  
例えば下記のようなコードです[^th-instance]👇

[^th-instance]: 現在もそうですが、実際にはTemplate Haskellで定義されているので、typesafe-precureのリポジトリーにはこれと全く同じコードはありません。

```haskell
data CurePeach = CurePeach

data CureStickPeachRod = CureStickPeachRod

instance Purification CurePeach CureStickPeachRod where
  purificationSpeech _ _ =
    [ "届け！愛のメロディ！"
    , "キュアスティック・ピーチロッド！"
    , "悪いの悪いの飛んでいけ！"
    , "プリキュア！ラブサンシャイン・フレッシュ！"
    ]
```

今回作った`cure-index.json`を最初に思いついたとき、「型クラスから各型のインスタンス宣言を自動で収集して、そこから`cure-index.json`を作れないだろうか」と、漠然と考えていました。  
typesafe-precureを作り始める以前、私はRubyで「定義を自動でまとめる問題」に対応した際、[Rubyでの場合](#typesafe-precure2_case-ruby)の節で紹介したような方法を用いていたため、「Haskellにおける、Rubyで言うところのmixi-inされるモジュールは型クラスだ」なんて類推をしていたからかも知れません。  
いずれにしても、そんな方法で実現できれば、既存のtypesafe-precureのモジュールの構造をそのまま使ってcure-indexが作れるので、大変都合がよかったのです。

しかし、残念ながらその方法は、少なくとも単純にTemplate Haskellを使うだけでは不可能であるとすぐ気づきました。  
なぜなら、[Template Haskellのライブラリーが提供する`reifyInstances`という関数](https://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH.html#v:reifyInstances)は、インスタンス宣言を取り出したい型を、自前で持ってきて引数として渡さなければならないからです。  
したがって、Rubyでやっていたように、型クラスのインスタンスを自動でリストアップする、といったことはできません<small>（もちろん、Rubyでやった時も完全に自動ではなく、`include`したクラスが自分でグローバルなリストに追加していたわけですが）</small>。  
それならば、自前で`import`しているモジュールから定義されている型を収集することはできないだろうか、と思って、指定したモジュールで定義されている型を取り出すAPIを探ってみましたが、それも見つかりませんでした。  
最もそれらしいことができそうな[`reifyModule`という関数](https://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH.html#v:reifyModule)が返す[`ModuleInfo`](https://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH.html#t:ModuleInfo)も、保持しているのはあくまでも`import`している別のモジュールだけであり、いくらreifyしてもモジュールの**中で**定義されている型の情報はとれないのです。

やむなく、私はtypesafe-precureの構造を改め、現在のような、JSONとして書き出すデータ構造を元に型と型クラスのインスタンスを自動で定義するような実装にすることとしました。  
この変更は依然として続いています。具体的には、今年新しく追加された「キラキラ☆プリキュアアラモード」に登場するプリキュア以外は、まだ従来の構造のままで、中間データの値は定義されていません。  
「キラキラ☆プリキュアアラモード」に収録されたプリキュアの情報しか、`cure-index.json`に記録されていないのはそのためです。

来年のプリキュアハッカソンやプリキュアAdvent Calendarでは、[haskell-src-exts](https://hackage.haskell.org/package/haskell-src-exts)という、HaskellでHaskellのソースコードをパースするライブラリーを使って、この大きな移行プロジェクトに取り組むことになるかと思います。  
typesafe-precureには技術的なネタが尽きませんね。

# ほかにやればよかったかも知れない方法

同じことを繰り返しますが、これから紹介する方法も含めて「定義を自動でまとめる」問題の解決は、どんな方法を使うにしても、多かれ少なかれ凝ったメタプログラミングのテクニックを使わなければならなくなります。  
[注意点](#typesafe-precure2_warnings)の節で強調したとおり、そのコードベースを初めて読んだ人が迷子にならないよう配慮することは忘れないでください。

## モジュールが持っている特定の名前の関数・型を処理する

その方法は、先の節でも紹介した[`hspec-discover`](https://hackage.haskell.org/package/hspec-discover)でも実際に行われている方法です。  
`hspec-discover`は、GHCのカスタムプリプロセッサーを利用して実行することで、テストが書かれたディレクトリーから`Spec`という名前で終わるすべてのテスト用モジュールを自動でまとめて、それらをすべて実行する`Spec.hs`を、自動で生成します。  
`hspec-discover`の場合、`ANN`のようなアノテーションは一切使用せず、モジュールの名前やモジュールがエクスポートする名前に規約を設けることで「定義をまとめる対象」を検出しています。  
このように、`ANN`のような特別な「印」を着けずに純粋に名前だけで「定義をまとめる対象」を決めることもできます。  
実績もあり、同じような方法をとることは非常に簡単そうです。

しかし、個人的には[注意点](#typesafe-precure2_warnings)の節でも述べたとおり、「定義をまとめる」対象であることを表す「印」は、「定義をまとめる」対象のファイルの中にあった方が、わかりやすくていいと思います。  
確かに`hspec-discover`のように、公開されていて広く使用されているものであれば、使用したプロジェクトのコードを初めて読む人でも、すぐに理解できる場合が多いでしょう。「何がまとめられるのか」も比較的直感的ですしね。  
とはいえ、私が想定している、例えばアプリケーションのプラグインみたいな、もう少しローカルなコードベースである場合、「印」はより「印」らしいものであった方が、手がかりとして気づきやすいのではないかと思います。  

😕初めて「まとめられる」コードを含むファイルを目にして、どのように使用されるのか分からず戸惑う  
⬇️  
🤔`{-# ANN MarkedAsFoo #-}`という見慣れないコメントを見つけて、それでコードベースを検索してみる<small>（プラグマは多くのsyntax highlighterで普通のコメントより目立って見えるはずです）</small>  
⬇️  
💡`MarkedAsFoo`が着いたモジュールを実際に収集してまとめているコードを見つけて、理解する

という流れで「定義を自動でまとめる」機構の存在に気づくのではないでしょうか。

あるいはいっそ`ANN`も使わずに、こんな内容のhuman-readableなコメントを「印」とするのもよいかも知れません。  
プログラムで検出するのもそう難しくはないでしょう。

```haskell
module Foo.Commands.SampleCommand where

-- | このコメントが付いたモジュールの 'execute' という関数は、
--   Template Haskellによって、自動的に再利用できるよう収集される。
--   詳しくは 'Foo.Commands` を読まれたし。
```

これなら、`Foo.Commands`モジュールにヒントがあることが、すぐに分かります。  
`hspec-discover`のように、Template Haskellを使わず直接ファイルシステムにあるファイルを開く方法とも、相性がいいはずです。

ほかにもいろいろな方法を考えましたが、これ以上に有効でもなさそうだし、そろそろ時間もなくなってきたので、この辺でまとめたいと思います。

# まとめ

- 「定義を自動でまとめる」問題を解決することにより、モジュールに関わる情報<small>（どのような定義で、どのように使用されるのか）</small>をなるべくモジュールのファイルのみに集約させることができる
- 「定義を自動でまとめる」問題を解決するには、下記のことをする
    - 「まとめたい定義」が書かれているファイルが、どのディレクトリー以下にあるか設定する
    - 「まとめたい定義」が書かれたファイルに、なんらかの印をつける
- Haskellで「定義を自動でまとめる」問題を解決する場合、Template HaskellとGHCの`ANN`プラグマや、GHCのカスタムプリプロセッサー(`-F -pgmF`)を組み合わせて使うことによって解決できるが、実際にはGHCのカスタムプリプロセッサーのみで十分可能
    - まとめる対象や状況に応じて、柔軟にやり方を考えよう
- どのような方法であれ、「定義を自動でまとめる」問題を解決すると、「『「自動でまとめられるファイル』がどのように使用されるか理解しにくくなる」という別の問題が発生するので、気をつけよう

それでは2018年もTemplate HaskellとプリキュアでHappy Hacking!! ❤️❤️❤️

# 参考にしたページ

（記事中で直接リンクを張っていないもののみ）

- [第2回 Springの様々な設定記述 – AnnotationもJavaもあるんだよ ｜ Developers.IO](https://dev.classmethod.jp/server-side/java/various-spring-configuration/)
- [instance method Module#included (Ruby 2.4.0)](https://docs.ruby-lang.org/ja/2.4.0/method/Module/i/included.html)
- [GHC User's Guideの「7.13. Pragmas」](https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/pragmas.html#annotation-pragmas)
