---
title: モナドの新しい力！スーパープリキュアモナド！
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
subHeading: ～タイプセーフプリキュア！を支える技術 その4～
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: December 25, 2018
tags:
...
---

# この記事は

この記事は[Haskell Advent Calendar その2](https://qiita.com/advent-calendar/2018/haskell2)兼[プリキュアAdvent Calendar 2018](https://adventar.org/calendars/2984)5日目の記事です。  
毎度同時投稿で失礼します。  
今年は私用で忙しかったので、のんびり書いてできあがったら空いてる日に投稿する、という楽なスタイルで書かせていただきました。なのでタイムスリップして5日目の記事と言うことにします<small>（それにしてもずいぶん時間かかってしまってすみません、もうクリスマスも過ぎたし...😥）</small>。

今回も例年の私のAdvent Calendarどおり、[タイプセーフプリキュア！](https://github.com/igrep/typesafe-precure)に、最近追加しようとした機能と、その際使用したもろもろの要素技術についての記事です。  
タイプセーフプリキュア！そのものについては[今年9月の記事](https://haskell.jp/blog/posts/2018/substring-parser.html)や、そこで言及しているもっと古い記事をご覧ください。

# 課題: プリキュアに変身していない状態で浄化技を使おうとした場合、型エラーにしたい

従来より、タイプセーフプリキュア！には、`PreCureMonad`と呼ばれる、プリキュアの台詞を`do`記法で組み立てる機能があります。  
例えばGHCi上で下記のように書くだけで、[「Go! プリンセスプリキュア」のあの名シーン](https://www.youtube.com/watch?v=oQLIyIZ2vk0)を再現できます[^youtube]。

[^youtube]: リンク先の動画をご覧になればわかるとおり、実際のそのシーンより大幅に省略されているところは突っ込まないでいただきたい...🙏。

```haskell
> :m ACME.PreCure
> :{
> let scene = do
>       say "この罪を抱いたまま、もう一度、グランプリンセスを目指す！"
>       scarlet <- transform Towa (PrincessPerfume DressUpKeyScarlet)
>       scarletModeElegant <- transform scarlet (PrincessPerfume DressUpKeyPhoenix)
>       purify scarletModeElegant (ScarletViolin DressUpKeyPhoenix)
> :}
```

名シーンを単純な文字列のリストとして使いたい場合はこう👇しましょう<small>（出力は手で見やすく加工しています）</small>。

```haskell
ghci> composeEpisode scene
[ "この罪を抱いたまま、もう一度、グランプリンセスを目指す！"
, "プリキュア！プリンセスエンゲージ！"
, "深紅の炎のプリンセス！キュアスカーレット！"
, "冷たい檻に閉ざされた夢、返していただきますわ。"
, "お覚悟を決めなさい！"
, "エクスチェンジ！モードエレガント！"
, "スカーレット・バイオリン！フェニックス！"
, "羽ばたけ炎の翼！"
, "プリキュア！ フェニックス・ブレイズ！"
, "ごきげんよう。"
]
```

さらに`printEpisode`という関数で実行すれば、1行ごとに間隔を置いてあの台詞を再生できます。

```haskell
ghci> printEpisode scene
この罪を抱いたまま、もう一度、グランプリンセスを目指す！
プリキュア！プリンセスエンゲージ！
深紅の炎のプリンセス！キュアスカーレット！
冷たい檻に閉ざされた夢、返していただきますわ。
お覚悟を決めなさい！
エクスチェンジ！モードエレガント！
スカーレット・バイオリン！フェニックス！
羽ばたけ炎の翼！
プリキュア！ フェニックス・ブレイズ！
ごきげんよう。
```

そんな`PreCureMonad`ですが、先ほどのコードをよく読めばわかるとおり、ちょっと不格好ですよね。  
具体的には下記の2行です。

```haskell
scarlet <- transform Towa (PrincessPerfume DressUpKey_Scarlet)
scarletModeElegant <- transform scarlet (PrincessPerfume DressUpKeyPhoenix)
```

1行目の`transform`関数が、変身する女の子である`Towa`（赤城トワ）と変身アイテムを受け取って`CureScarlet`を返し、さらにその`CureScarlet`を2行目の`transform`関数に渡すことでキュアスカーレットのモード・エレガント（`CureScarlet_ModeElegant`）を取得しています。  
「`transform`関数が、変身する女の子である`Towa`（赤城トワ）と変身アイテムを受け取って`CureScarlet`を」返すという箇所について、`Towa`に**加えて**`CureScarlet`を**新しく作っている**ように聞こえます。  
本来同一人物であるはずの`Towa`と`CureScarlet`を、あたかも別々のものとして扱っているように捉えられ兼ねません。  
そう、本来プリキュアの「変身」は女の子自身の状態を書き換えるものとして表現した方が自然なのです。

Haskellでそうした「状態」を表現する場合、名前のとおりState Monadを使うのが割と一般的な方法です<small>（プログラム全体で状態を管理する場合、`IORef`や`TVar`などを使う方が例外に強く安全ではありますが、それはさておき）</small>。  
しかし、従来のState Monadでプリキュアの変身や浄化技を表現する場合、**女の子が変身していない状態で浄化技(`purify`)を使おうとした場合をどのように扱うか**、という問題があります。  
先ほどの例で言うところの

```haskell
purify scarletModeElegant (ScarletViolin DressUpKeyPhoenix)
```

という行でまさにその「浄化技」を実行しているのですが、プリキュアの設定上、特定の浄化技を使うには、特定のプリキュアのフォームに、専用のアイテムを渡さなければなりません。  
タイプセーフプリキュア！ではこの点に強くこだわり、浄化技が使用できる組み合わせごとに型クラスのインスタンスを定義することで、間違った組み合わせを`purify`関数に渡すと、型エラーになります<small>（詳しくは[タイプセーフプリキュア！を最初に技術的に解説した記事](https://qiita.com/igrep/items/5496fa405fae00b5a737)をご覧ください）</small>。  
当然、まだ変身していない状態の女の子を`purify`関数に渡しても、エラーになってしまいます。

```haskell
> scene = purify Towa (ScarletViolin DressUpKeyPhoenix)

<interactive>:4:9: error:
    • No instance for (Purification
                         Towa (ScarletViolin DressUpKeyPhoenix))
        arising from a use of ‘purify’
    • In the expression: purify Towa (ScarletViolin DressUpKeyPhoenix)
      In an equation for ‘scene’:
          scene = purify Towa (ScarletViolin DressUpKeyPhoenix)
```

プリキュア実装の大先輩である[rubicure](https://github.com/sue445/rubicure)では、同じようなケースで実行時エラーを出すようにしていますし、PreCure Monadにおいても、`ExceptT`を使ってエラーにする、という方法が採れるでしょう。  
しかしそこは「タイプセーフプリキュア！」。どうにかして、変身していない状態での`purify`関数の実行を型エラーにして、従来のこの振る舞いと一貫させたいところですよね。  
というのが今回の課題です。

# 実現方法: Indexed Monadと型レベル連想配列を使う

今回の課題のとおり、「変身していない状態での`purify`関数の実行を型エラー」としつつ、「変身した状態での`purify`を型エラーとしない」ためには、`purify`や`transform`を実行する前後で、`State` Monad内で共有している値の型を変更できるようにする必要があります。  
残念ながら、これは従来の`State` Monadでは不可能です。  
`State s`に対する`>>=`の型が`(>>=) :: State s a -> (a -> State s b) -> State s b`となっていることから察せられるとおり、`State` Monadの中で共有する型は、アクションの実行前後にかかわらず同じ`s`でないといけないためです。  
これはそもそも従来のMonadの仕様上やむを得ないことです。  
従来のMonadはそもそもアクションの実行前後で、アクションの実行結果以外の型を変えることができないようになっています。  
`>>=`の型が`(>>=) :: Monad m => m a -> (a -> m b) -> m b`となっていることからしても、アクションの実行前後で`m`は`m`のままであることがわかります。

この、「アクションの実行前後で、`m`の型を変えることができる」ようにしたのがIndexed Monadです。  
Indexed Monadは次のような型宣言にすることで、アクションの実行前後で異なる型の "index" を挟めるようになっています。

```haskell
class IxApplicative m => IxMonad m where
  ibind :: (a -> m j k b) -> m i j a -> m i k b
```

`IxApplicative`は名前のとおり`IxMonad`と同様に"index"が付いた`Applicative`となっています。[詳しい定義はドキュメント](http://hackage.haskell.org/package/indexed-0.1/docs/Control-Monad-Indexed.html)をご覧ください。

唯一のメソッドである`ibind`が普通の`Monad`の`>>=`に、"index"を追加したものです。  
`(>>=) :: Monad m => m a -> (a -> m b) -> m b`の`m`に、型引数が2つ追加されていますね？これが"index"です。  
ある`IxMonad` `m`が`m i j a`という形で型引数を渡されている時、`i`がアクションを実行する**前**の型、`j`がアクションを実行した**後**の型を表します。  
`a`は普通の`Monad`と同様、アクションの実行結果となっています。

さらにIndexed State Monad (`IxState`)で使えるアクションの型宣言を見れば、`IxState`で共有している状態の型が、アクションの実行前後で変更できることがよりはっきりとわかるでしょう。

```haskell
iget ::      IxState i i i
-- ^ igetしてもIxStateが管理している状態は変わらないため、型もやはり変わらず。

iput :: j -> IxState i j ()
-- ^ iputするとIxStateが管理している状態は、引数で渡した値の型に変わる。
```

こちらもおなじみ[mtlパッケージ](http://hackage.haskell.org/package/mtl)にある`State` Monadに、単純に "index" を加えただけのものとなっています。

[Indexed Monadの世界 - モナドとわたしとコモナド](http://fumieval.hatenablog.com/entry/2013/05/04/144840)で紹介された際のIndexed Monadは、`ido`というQuasi Quoteを使って`do`記法を無理矢理シミュレートしていましたが、現在はGHCの`RebindableSyntax`という拡張を使うことで、普通の`do`記法をそのまま利用することができるようになりました<small>（例は後で紹介します）</small>。  
さらに、現在は`RebindableSyntax`を使った場合の諸々の問題を回避するべく、[Indexed Monadを一般化したSuper Monadと、それを簡単に使えるようにしたGHCの型チェッカープラグイン](https://github.com/jbracker/supermonad)が作られたり、[do-notationという、Indexed Monadと普通のMonadを型クラスで抽象化したパッケージ](https://github.com/isovector/do-notation)が作られたりしています。  
今回は純粋にIndexed Monadを使うだけで十分だったので、Super Monadやdo-notationは使用しませんでしたが、今後Indexed Monadをもっと実践的に使用する機会があれば、使用してみたいと思います。

Indexed Monadを使用することで、`State` Monadで共有している状態の型を、アクションの実行前後で変更できるようになりました。  
続いて、各女の子の状態を、`State` Monadで共有している状態の型として、どのように管理するかを検討しましょう。  
というのも、タイプセーフプリキュア！には最新のmasterの時点で59人の女の子が収録されている[^allstars]のですが、それらすべてを変身前と変身後に分けて管理するだけでも、2 ^ 59通りの状態を型として表現できなければなりません。  
これを直感的に表現できるようにするために、ちょっと型レベルプログラミングの力を借りましょう。そこで登場するのが「型レベル連想配列」です。  
「型レベル連想配列」という言い方はあまりしないのでピンとこないかも知れませんが、要するに型<small>（タイプセーフプリキュア！の場合、プリキュアに変身する女の子一人一人に個別の型を割り当てているので、その個別の型）</small>と、それに対応する値のペアを含んだ型レベルリストです。  
大雑把に言うと、下記👇のような内容となります<small>（実際にはもう少し違う型で構成されています）</small>。

```haskell
[ (Hana,   HasTransformed 'True)
-- ^ プリキュアに変身する女の子を表す型（この場合「HUGっと！プリキュア」の野乃はな）

, (Saaya,  HasTransformed 'False)
--         ^ 対応する女の子が変身しているかどうかを表すsingleton type。
--           DataKindsで型に持ち上げられたBoolを、普通の値として扱えるよう変換するためのラッパー。
--           申し訳なくもsingleton typeについては割愛します。
--           Haskell-jpのSlack Workspaceあたりでリクエストがあったら書こうかな。

, (Homare, HasTransformed 'False)
, ...
]
```

[^allstars]: 「この間の映画に出ていた人数より多くない？」と思った方へ: 「プリキュアオールスターズ」に出てくる女の子たちに加え、坂上あゆみ、ペコリン、若宮アンリ、はぐたんの4人が、タイプセーフプリキュア！の分類上`Girl`として収録されています。ちなみにキュアモフルンも収録されていますが、モフルンはあくまでも変身アイテム(`SpecialItem`)という扱いです。

別の視点で見ると、これはいわゆるExtensible Recordとも似ています。  
[extensibleパッケージ](http://hackage.haskell.org/package/extensible)や[labelsパッケージ](https://github.com/chrisdone/labels)、[superrecordパッケージ](https://www.athiemann.net/2017/07/02/superrecord.html)がそうしているように、Extensible Recordは、フィールドのラベルを表す<small>（型レベルの、静的な）</small>文字列をキーとして、それに対応する値を含んだ連想配列として表現することができるためです。  
事実私は今回、extensibleを使ってこの機能を実装しました。他のExtensible Recordの実装でも良かったのですが、これ以外のものを全く使ったことがないので😅。

# できたもの

Indexed MonadとExtensible Recordを組み合わせることで、PreCureMonadの各種アクションを、次のように置き換えられることがわかりました。  

- `transform <girl> <item>`:
    - `IxState`（実際にはそのMonad Transformer版である`IxStateT`）で共有している型レベル連想配列のキー`<girl>`に対応する値を「変身した状態」に更新する。
    - `<girl>`がすでに変身している状態の場合は、型レベル連想配列のキー`<girl>`に対応する値が「変身した状態」になっているので型エラーとする。
    - `IxStateT`をかぶせた`Writer` Monadで共有しているリストに、`<girl>`と`<item>`に対応した、変身時の台詞（文字列）を追記する。
- `purify <precure> <item>`:
    - `IxStateT`で共有している型レベル連想配列のキーを取得するため、`<precure>`にあらかじめ定義しておいたType Family `AsGirl`を適用する。
        - `AsGirl`で取得した型を、これ以降`<girl>`と呼びます。
    - `<girl>`が「変身した状態」になっていない場合は、型レベル連想配列のキー`<girl>`に対応する値が「変身していない状態」になっているので型エラーとする。
    - `IxStateT`をかぶせた`Writer` Monadで共有しているリストに、`<precure>`と`<item>`に対応した、浄化技を使用したときの台詞（文字列）を追記する。

このように生まれ変わったPreCure Monadを**✨Super PreCure Monad✨**と呼ぶこととします💪

下記がSuper PreCure Monadのサンプルコードです。  
野乃はながキュアエールに変身して、「ハート・フォー・ユー」という浄化技を放つまでを表しています。

```haskell
cureYell :: PreCureM (StatusTable '[]) (StatusTable '[Hana >: HasTransformed 'True]) ()
cureYell = do
  enter Hana
  transform Hana (PreHeart MiraiCrystalPink)
  purify CureYell (PreHeart MiraiCrystalPink)
```

`enter`は、旧PreCureMonadにはない、Super PreCure Monadに新しく追加されたアクションです。  
引数で指定された女の子や、女の子が変身したプリキュアを「登場」させます。  
具体的には、以下のように振る舞います。

- 引数で指定された値が女の子`<girl>`であれば、`IxStateT`で共有している型レベル連想配列のキー`<girl>`に対応する値を「変身していない状態」で追加する。
- 引数で指定された値がすでに変身したプリキュア`<precure>`であれば、`<precure>`にType Family `AsGirl`を適用し、女の子を表す値`<girl>`を取得する。
    - `IxStateT`で共有している型レベル連想配列のキー`<girl>`に対応する値を「変身した状態」で追加する。

したがって、`transform`するにしても`purify`するにしても、事前に変身前の女の子かその変身後のプリキュアが`enter`していないといけません。  
これは単純にその方が実装が簡単だから、という理由もありますし、一旦「登場」させたほうがなんとなくかっこいいかな、と感じたからです。

## ✨Super PreCure Monad✨を試す方法

ここまで述べたような基本的な仕様は実装できたものの、まだ解決すべき技術的な問題が見つかったので、残念ながらリリースはされていません<small>（その詳細は気が向いたら書きます）</small>。  
なので、試す場合は下記のように実行してください。

```haskell
$ chcp 65001
-- ^ Windowsの方は恐らく必要

$ git clone -b super-precure-monad https://github.com/igrep/typesafe-precure.git
$ cd typesafe-precure
$ stack build
$ stack exec ghci
> :set -XRebindableSyntax -XFlexibleContexts -XTypeFamilies
> import Prelude hiding ((>>), (>>=))
> :m + ACME.PreCure ACME.PreCure.Monad.Super
> :{
> scene = do
>       enter Makoto
>       transform Makoto (LovelyCommuneDavi CureLoveads)
>       purify CureSword (LovelyCommuneDavi CureLoveads)
> :}
> printEpisode scene
(ダビィー！)
プリキュア！ラブリンク！
(L! O! V! E!)
勇気の刃！ キュアソード！
このキュアソードが 愛の剣で、あなたの野望を断ち切ってみせる！
閃け！ホーリー・ソード！
```

「変身していない状態での`purify`関数の実行を型エラーとする」といった仕様を試す場合は、[こちらに置いた、全プリキュアの変身と浄化技を列挙したテスト用ファイル](https://github.com/igrep/typesafe-precure/blob/super-precure-monad/gen/AllPreCureM.hs)をghciで読んでみるといいでしょう。  
先ほど👆の手順で`git clone`したディレクトリーにおいて、あらかじめ`stack build`を実行しておくのをお忘れなく。

```
$ stack build
$ stack exec ghci gen/AllPreCureM.hs
```

適当に`gen/AllPreCureM.hs`を書き換えて`:r`してみれば、概ねいい感じに動いていることがわかるはずです。

例えば冒頭付近にある、

```haskell
act_CureDiamond_LovelyCommuneRaquel_CureLoveads = printEpisode $ do
  enter Rikka
  transform Rikka (LovelyCommuneRaquel CureLoveads)
  purify CureDiamond (LovelyCommuneRaquel CureLoveads)
```

というSuper PreCure Monadによるアクションから、`transform Rikka (LovelyCommuneRaquel CureLoveads)`という行を削除した上で`:r`してみると、次のようなエラーになります。

```
> :r
[1 of 1] Compiling AllPreCureM      ( gen\AllPreCureM.hs, interpreted )

gen\AllPreCureM.hs:22:3: error:
    • Couldn't match type ‘'False’ with ‘'True’
        arising from a use of ‘purify’
    • In a stmt of a 'do' block:
        purify CureDiamond (LovelyCommuneRaquel CureLoveads)
      In the second argument of ‘($)’, namely
        ‘do enter Rikka
            purify CureDiamond (LovelyCommuneRaquel CureLoveads)’
      In the expression:
        printEpisode
          $ do enter Rikka
               purify CureDiamond (LovelyCommuneRaquel CureLoveads)
   |
22 |   purify CureDiamond (LovelyCommuneRaquel CureLoveads)
   |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Failed, no modules loaded.
```

ちゃんと、変身していない状態で`purify`することを型エラーにできていますね！

ここまでできていながら残念ですが、リリースは、来年のプリキュアハッカソンかAdvent Calendarあたりに乞うご期待と言うことで！💦  
それでは2019年もHaskellでSuper PreCure Hackingを❣️❣️❣️
