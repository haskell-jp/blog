---
title: "regex-applicative: 内部DSLとしての正規表現（ブログ記事版）"
subHeading: RegexFestaで発表した内容を詳しく紹介します
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: November 3, 2019
tags: 正規表現
...
---

先日私は[Regex Festa](https://opt.connpass.com/event/140566/)というイベントで、「[regex-applicative](http://hackage.haskell.org/package/regex-applicative)」というパッケージの紹介を致しました。  
今回は[その際使用したスライド](https://the.igreque.info/slides/2019-10-18-regex-applicative.html)を、ブログ記事として詳しく共有させていただきたいと思います！  
発表時のスライドと比べて、よりHaskellを知っている人向けになってしまいますが、regex-applicativeの魅力を明確に伝えるために必要なのでご了承ください。  
Applicativeスタイルを前提知識とします。

# はじめにまとめ

- regex-applicativeは、Haskellの式で正規表現を書ける内部DSL
- パーサーコンビネーターっぽく使えて、かつ正規表現の良さ --- 中間マッチが簡単にできる点など --- を持ち合わせている
- 内部は「文字を受け取って続きの状態のリストを返す関数」として表現されたNFAで実装されている

# regex-applicativeって？

[regex-applicative](http://hackage.haskell.org/package/regex-applicative)は、正規表現をHaskellの内部DSLとして表現したライブラリーです。  
名前のとおり、いわゆる「`Applicative`スタイル」で正規表現を書くことができます。

# regex-applicativeのAPI概要

regex-applicativeには、正規表現オブジェクト[`RE`型](http://hackage.haskell.org/package/regex-applicative-0.3.3.1/docs/Text-Regex-Applicative.html#t:RE)の値とマッチさせる文字列を受け取って、その結果を返す関数がいくつかあります。  
今回はそのうち最も単純な`match`関数を使用します。👇のような型定義となっています。

```haskell
match :: RE s a -> [s] -> Maybe a
```

定義のとおり、`RE`型は型引数としてマッチさせる文字の型`s`と、マッチした結果にも使われる「正規表現の結果」を表す型`a`を受け取ります。  
`RE`型を`Applicative`のインスタンスにするためには、その結果を表す型が必須なのです。この後出す例でこの「正規表現の結果」を好きな値に変える方法を示しましょう。

そして第2引数がマッチさせる文字列に当たります。`[s]`と`RE`型の第1型引数`s`のリストになっているとおり、`match`関数<small>（と、その他のregex-applicativeにおいて文字列をマッチさせるAPI）</small>は任意のリストに対して使用することができます。  
Haskellの標準の文字列`String`の実態は`[Char]`、すなわち`Char`のリストなので、通常regex-applicativeを使用する場合`s`には`Char`が割り当てられます。  
型変数なので、当然他の型のリストに対しても使用できます。これは他の正規表現ライブラリーではあまりない特性でしょう。

戻り値はおなじみの`Maybe`型です。マッチが成功すれば、引数に渡した正規表現`RE s a`型の「結果」、`a`型の値を`Just`にくるんで返します。そして失敗すればもちろん`Nothing`を返します。

⚠️`match`関数について特筆すべきことをもう一つ。他のよくある正規表現ライブラリーと異なり、`match`関数は完全一致じゃないとマッチしないのでご注意ください。  
regex-applicativeには完全一致じゃない関数とそうでない関数両方があるので、少し混乱します😰

# regex-applicativeの使用例

それではいよいよregex-applicativeパッケージを使ってみましょう。  
👇のコマンドでインストールして、GHCiで試します。

```bash
stack build regex-applicative
stack exec ghci
```

<small>（最近の）</small>`cabal`の場合は👇を実行すればできるはずです。

```bash
cabal v2-install --lib regex-applicative
cabal v2-repl -b regex-applicative
```

GHCiが起動したら、こちらの`import`文を張って、本記事のサンプルを実行する準備をしてください。

```hs
import Text.Regex.Applicative
import Text.Regex.Applicative.Common
```

## ただの文字: `sym :: Eq s => s -> RE s s`

ここからは、正規表現の基本的な機能を利用するためのregex-applicativeのAPIを紹介します。  
まずはただの文字一つにマッチする`sym`から:

```haskell
> match (sym 'a') "a"
Just 'a'

> match (sym 'a') "b"
Nothing
```

`sym :: Eq s => s -> RE s s`という型定義のとおり、引数として受け取った文字と文字列における文字が等しいかチェックして、等しければマッチした文字をそのまま返す正規表現を作ります。

また、より一般化したバージョンとして、`psym`という関数もあります。  
こちらは`psym :: (s -> Bool) -> RE s s`という型定義のとおり、「文字を受け取ってブール値を返す関数」を受け取って、受け取った関数が文字に対して`True`を返したらマッチする、という正規表現を作ります。

なので例えば、

```haskell
> match (psym (== 'a')) "a"
```

と書けば`sym`関数と全く同じことができますし、

```haskell
> match (psym (`elem` "abcdef")) "a"
Just 'a'
> match (psym (`elem` "abcdef")) "b"
Just 'b'
```

と書けば、文字クラスっぽいことができます。

## 空文字（ε）: `pure :: a -> RE s a`

正規表現に欠かせない、空文字（ε）を表す正規表現も作れます。  
`Applicative`型クラスの`pure`で表現します。

```haskell
> match (pure 'a') ""
Just 'a'
> match (pure 'a') "b"
Nothing
```

もちろん、`pure`は任意の値を受け取って「受け取った値をそのまま返すもの」を作ることができるので、結果として文字（列）以外の値を返す正規表現も、簡単に作ることができます。

```haskell
> match (pure True) ""
Just True
> match (pure 42) ""
Just 42
```

## 連接: `(*>) :: RE s a -> RE s b -> RE s b`・`string :: Eq a => [a] -> RE a [a]`

続いて連接、つまり「二つ以上の正規表現を続けてマッチさせる正規表現を作る」処理です。  
regex-applicativeでは、`Applicative`型クラスの`*>`がそのまま連接として使えるようになっています。

```haskell
> match (sym 'a' *> sym 'b') "ab"
Just 'b'
```

当然、単なる文字の正規表現を並べることはありふれたことなので、`string`関数というショートカットが用意されています。

```haskell
-- マッチする文字列は同じ、より分かりやすいバージョン
> match (string "ab") "ab"
Just "ab"
```

さらに、regex-applicativeの正規表現オブジェクトは[`IsString`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-String.html#t:IsString)型クラスのインスタンスでもあるので、`OverloadedStrings`言語拡張を使えば文字列リテラルだけで正規表現オブジェクトを作ることができます。

```haskell
> :set -XOverloadedStrings
> match "ab" "ab"
Just "ab"
```

## 選択: `(<|>) :: RE s a -> RE s a -> RE s a`

正規表現の「選択」、すなわち「二つの正規表現のうちどちらか一方にマッチする正規表現を作る」処理は、[`Alternative`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html#t:Alternative)型クラスでおなじみの`<|>`を使います[^alternative]。

[^alternative]: `Alternative`は、`Applicative`より強力な（できることが多い）型クラスです。そういう意味で、regex-applicativeは本当は「regex-alternative」と呼んだ方が適切なのかも知れません。

```haskell
> match (sym 'a' <|> sym 'b') "a"
Just 'a'
> match (sym 'a' <|> sym 'b') "b"
Just 'b'
```

## 繰り返し: `many :: RE s a -> RE s [a]`・`some :: RE s a -> RE s [a]`

正規表現の「繰り返し」、指定した正規表現を繰り返しマッチさせる正規表現を作る処理は、これまた`Alternative`の`many`メソッド・`some`メソッドによって実装されています。  
`Alternative`型クラスのデフォルトの定義どおり、`many`が0回以上の繰り返し、`some`が1回以上の繰り返しを表しています。

```haskell
> match (many (sym 'a')) "aaaaaaaaaa"
Just "aaaaaaaaaa"
> match (some (sym 'a')) "aaaaaaaaaa"
Just "aaaaaaaaaa"

> match (many (sym 'a')) ""
Just ""
> match (some (sym 'a')) ""
Nothing
```

## オプショナルなマッチ: `optional :: RE s a -> RE s (Maybe a)`

それから、いわゆる「正規表現の基本三演算」には含まれてませんが<small>（選択と`pure`で実装できるので）</small>、この後の例で使用するので「オプショナルなマッチ」を実現する方法を紹介しておきます。  
名前のとおり`optional`という関数を使います。これも`Alternative`型クラスに対して使える関数ですね！

```haskell
> match (sym 'a' *> optional (sym 'b')) "ab"
Just (Just 'b')
> match (sym 'a' *> optional (sym 'b')) "a"
Just Nothing
```

## マッチした結果をHaskellの値に割り当て

ここからは、他の正規表現ライブラリーでは珍しい、「正規表現でマッチした結果をHaskellの値に割り当てる方法」をより詳しく紹介します。

### 組み込みの正規表現を使う

例えば、[`Text.Regex.Applicative.Common`モジュールにある`digit`](http://hackage.haskell.org/package/regex-applicative-0.3.3.1/docs/Text-Regex-Applicative-Common.html#v:digit)は、一桁の数字<small>（つまり`0`から`9`）</small>にマッチした上で、結果としてマッチした値を**文字ではなく、整数として**返す正規表現を提供します。

```haskell
> match digit "1"
Just 1
```

加えて、先ほど紹介した`many`関数と組み合わせると、マッチした結果を整数のリストとして取得することもできます。

```haskell
> match (many digit) "12345"
Just [1,2,3,4,5]
```

### `(<$>) :: (a -> b) -> RE s a -> RE s b`: 任意の（一引数の）関数を適用する

regex-applicativeは、名前のとおり正規表現をApplicativeスタイルで利用できるようにするためのライブラリーです。  
当然ながら`Applicative`スタイルに必須の`<$>`関数も使用できます。  
正規表現オブジェクト`RE s a`型の返す「マッチした結果」に、あなたの好きな関数を適用して変換した正規表現を作れるのです。

先ほどの`many digit`の例を再利用して、マッチした整数の合計値を求めてみましょう。

```haskell
> match (sum <$> many digit) "12345"
Just 15
```

### `(<*>) :: RE s (a -> b) -> RE s a -> RE s b`: 任意の関数を適用する

Applicativeスタイルのもう一つの重要な関数といえば、やっぱり`<*>`でしょう。  
`many digit`を再利用して、「先頭に書かれた桁数だけ数字を取得する」という例を書いてみます。

```haskell
> match (take <$> digit <*> many digit) "312345"
Just [1,2,3]
```

## もうちょっと複雑な例

ここまで紹介した例を使用してもうちょっと複雑な例を書いてみましょう。  
小さな正規表現を組み合わせて、httpかhttpsのURLにおける、オリジンにマッチする正規表現を簡単に書いてみます。

まずは部品作りです。

URLのスキームにマッチさせるために、「`http`の後にオプショナルな`s`、続けて`://`」という文字列にマッチする正規表現を作ります。

```haskell
> schemeRe =
    ((++) <$> string "http" <*> (string "s" <|> pure ""))
      <* string "://"
```

`<*`を使うことで、`://`の部分にはマッチしてもマッチした結果は無視している点にご注意ください。  
regex-applicativeはこのように、「マッチしたら関数に渡す文字列」と「マッチしても関数に渡さない文字列」をユーザーが書き分けられるようになっているので、他の正規表現ライブラリーにあるようなキャプチャー[^capture]や、先読み言明・後読み言明などの機能が必要ないのです。

[^capture]: 正確には、キャプチャーした文字列を正規表現の中で再利用することができないので、他の正規表現ライブラリーのキャプチャー機能と完全に同等のことができるわけではありません。これは現状のregex-applicativeの制限です。

続けて、ホスト名にマッチする正規表現を作ります。  
ここでは単純化して、「アルファベットの小文字かピリオド1文字以上」という文字列にしておきます。

```haskell
> hostRe = some (psym (`elem` ['a'..'z'] ++ "."))
```

最後はポート番号です。  
`:`という文字の後に[`Text.Regex.Applicative.Common`に入った`decimal`](http://hackage.haskell.org/package/regex-applicative-0.3.3.1/docs/Text-Regex-Applicative-Common.html#v:decimal)、すなわち一桁以上の10進数にマッチする正規表現を使います。

```haskell
> portRe = sym ':' *> decimal
```

以上で正規表現のパーツができました。  
早速使ってみる... 前に、マッチした結果を割り当てるレコード型を定義します。

```haskell
data Origin =
  Origin { scheme :: String, host :: String, port :: Int }
  deriving Show
```

あとは`<$>`や`<*>`を使って組み合わせて、`Origin`値コンストラクターに食わせるだけです！  
ポート番号はオリジンにおいてはなくても良いので、省略した場合は仮に`80`としておきましょう[^https]。

[^https]: もちろん、実際のところhttpsの場合デフォルトのポート番号は443であるべきですが、ちゃんと実装しようとすると結構複雑になるのでご容赦を！

```haskell
originRe = Origin   <$>
           schemeRe <*>
           hostRe   <*>
           (portRe <|> pure 80)
```

今度こそ使ってみます。

```haskell
> match originRe "https://example.com:8080"
Just (Origin {scheme = "https", host = "example.com", port = 8080})
> match originRe "http://example.com"
Just (Origin {scheme = "http", host = "example.com", port = 80})
```

regex-applicativeを使うことで、URLのオリジンにマッチさせるだけでなく、マッチした結果を`Origin`型の値として割り当てる正規表現が作れました！🎉

# 👍regex-applicativeのメリット

regex-applicativeパッケージには、他の正規表現ライブラリーと比べて、以下のメリットがあります。

- 文字列以外の扱いにも強い
    - マッチした結果から（文字列以外の）Haskellの値に割り当てるのが簡単！
        - 「生のデータ」からアプリケーションにおける「コアの処理が欲しいデータ」への変換がワンストップ
    - 文字列だけでなく、任意のリストに対してマッチできる
- 内部DSLとして書けるので、コンパイラーによる型チェックの恩恵を受けやすい
    - 前述の「マッチした結果から（文字列以外の）Haskellの値に割り当てる」処理も、すべて型チェックされる

# 👎regex-applicativeのデメリット

一方regex-applicativeパッケージには、他の正規表現ライブラリーに対する以下のデメリットがあります。

- コードは長い
    - 内部DSLなのでやむなし
    - 専用のメタキャラクターより分かりやすい、とも言える
- ユーザーからの入力として、正規表現を受け取ることは難しい
    - これも内部DSLなのでやむなし
- おそらくCとかで書いたものほど速くはない
    - そんなに細かい最適化をしているわけではないし、Pure Haskellなので...
- `String`以外の文字列にはマッチできない...
    - これがHaskellerにとって一番痛い
    - `Text`や`ByteString`向けのものも、原理的に実装できないというわけではないはず
    - 参考: [Haskell Tips (文字列編) - りんごがでている](http://bicycle1885.hatenablog.com/entry/2012/12/24/234707)

# ⚙️regex-applicativeの仕組み

ここからは、regex-applicativeにおける正規表現エンジンがどのように作られているか、『[正規表現技術入門](https://gihyo.jp/book/2015/978-4-7741-7270-5)』における正規表現エンジンの分類を参考に説明しましょう。

## 📑正規表現エンジンの分類

『正規表現技術入門』のp.56では、正規表現エンジンを次の二つに分類しています。

- DFA型
    - 正規表現を決定性有限オートマトン（deterministic finite automaton）と呼ばれるものに変換して正規表現マッチングを行う
- VM型
    - 正規表現をバイトコード（bytecode）と呼ばれるものに変換して正規表現マッチングを行う

さて、regex-applicativeの場合はどちらなのでしょうか？  
[ソースコード](https://github.com/feuerbach/regex-applicative/)を読んでみると、どうやらどちらでもなさそうなことがわかります。  
というのも、正規表現オブジェクト`RE s a`をNFAに[`compile`](https://github.com/feuerbach/regex-applicative/blob/5e9a06622d33c7657353ddaccfe101b96946027a/Text/Regex/Applicative/Object.hs#L110-L111)という関数で変換した後、DFAに変換しないでそのまま使っているからです。  
一般的に、NFAはDFAに変換可能で、変換してからマッチさせた方がしばしば高速にマッチできることが知られています。  
ところがregex-applicativeではその変換を行わず、NFAとして使用しているのです。

なぜそうした仕様になっているかについて、私の推測を述べましょう[^few]。  
regex-applicativeでは先ほど紹介した`psym`関数のように、「任意の文字を受け取る関数」を正規表現オブジェクトに含められなければなりません。  
結果、関数がどんな文字の時にどんな値を返すのか<small>（マッチが成功するのかしないのか）</small>、正規表現オブジェクトをコンパイルする関数にはわからなくなってしまうのです。  
一方、効率の良いDFAの実装では、DFAの一つ一つの状態ごとに「どの文字を受け取ったら次はどの状態に遷移するか」という情報を、連想配列として持っておかなければなりません[^dfa-impl]。  
そのため、どの文字を受け取ったらマッチが成功するのかわからない箇所が正規表現オブジェクトに混ざっている限り、効率の良いDFAの実装にはできないのです。

[^few]: この記事の最後の方を書いていて思い出しました。regex-applicativeはDFAベースの正規表現エンジンでは不可能な「控えめな繰り返し」をサポートしているから、という理由もあるようです。なぜDFAベースでは「控えめな繰り返し」ができないかは私もうまく説明できません...。
[^dfa-impl]: 『正規表現技術入門』のp. 132における実装例では、これを状態と文字による二次元配列として実装しています。

その分、regex-applicativeでは任意の文字を受け取る関数が使えるので、普通の正規表現ライブラリーよりも柔軟に書くことができるようになっています。  
その点を考慮したトレードオフなんでしょう。

## regex-applicativeの実際の実装

さらにregex-applicativeの実装を掘ってみましょう。  
先ほど紹介した`compile`関数は、正規表現オブジェクト`RE s a`を[`ReObject s r`](https://github.com/feuerbach/regex-applicative/blob/5e9a06622d33c7657353ddaccfe101b96946027a/Text/Regex/Applicative/Object.hs#L38-L43)という型の、[`Thread s r`](https://github.com/feuerbach/regex-applicative/blob/5e9a06622d33c7657353ddaccfe101b96946027a/Text/Regex/Applicative/Types.hs#L9-L16)型の値のキューに変換します。  
これがregex-applicativeにおけるNFAと呼べそうですね。

```haskell
newtype ReObject s r = ReObject (SQ.StateQueue (Thread s r))
```

`Thread s r`型の値は、NFAにおける状態遷移を表します。

```haskell
data Thread s r
    = Thread
        { threadId_ :: ThreadId
        , _threadCont :: s -> [Thread s r]
        }
    | Accept r
```

型定義のとおり、`Thread`と`Accept`という二通りの値をとります[^parconc-thread]。

[^parconc-thread]: 並行並列プログラミングで出てくるあの「スレッド」とは違うのでご注意ください。

- ⏩`Thread`はその用途からして、事実上`s -> [Thread s r]`という関数と同等の型です。regex-applicativeは`ReObject`によって文字列`[s]`の値をマッチさせる際、この`s -> [Thread s r]`に文字を渡します。
    - ➡️そして、関数が結果として返した、`Thread s r`型の値を<small>（そのリストから）</small>一つずつキューに追加して、また次の文字にマッチさせます。  
      <small>（キューなんで関数が返した新しい`Thread s r`型の値が直ちに実行されます）</small>
    - ↩️一方、関数が空リストを返した場合は --- そう、マッチが失敗した、ということなのです。その場合は、キューからさらに`Thread s r`の値を取り出して<small>（値コンストラクターが`Thread`であれば）</small>マッチしなかった文字をまた`s -> [Thread s r]`に渡します。
    - なお、`threadId_`はキューに追加する際同じ`threadId_`の`Thread`を追加してしまうのを防ぐためのキーです。詳細は割愛します。
- ✅`Accept r`は名前のとおりNFAの受理状態を表しています。`s -> [Thread s r]`を繰り返し適用して最終的に`Accept r`を返した`Thread`のみが「マッチした」と解釈されます。

このように、regex-applicativeにおけるNFAは`s -> [Thread s r]`を返す関数、すなわち「文字を受け取って次の状態のリストを返す継続」として作られています。

ただ実際に実行する際の流れを見てみると、`ReObject`に含まれる`Thread`を一つずつ実行してみて、結果が条件に合うものを選ぶ、といった方が近いです。  
例えば[`match`関数](https://github.com/feuerbach/regex-applicative/blob/5e9a06622d33c7657353ddaccfe101b96946027a/Text/Regex/Applicative/Interface.hs#L81-L85)では、`ReObject`に文字を一文字ずつ与えた結果の中から、`listToMaybe`を使って最初に`Accept`にたどり着く`Thread`を取得しています。  
それから、最長マッチする部分文字列を検索する[`findLongestPrefix`関数](https://github.com/feuerbach/regex-applicative/blob/5e9a06622d33c7657353ddaccfe101b96946027a/Text/Regex/Applicative/Interface.hs#L141-L149)は、マッチが失敗するか残りの文字列が空になるまで繰り返し文字を`ReObject`に与えることで、できるだけ長いマッチが返るように調整しています。  
このようにregex-applicativeは、`ReObject`(NFA)に文字を一つずつ与えてマッチ結果を生成する処理と、そのマッチ結果を選び取る処理とを分離することで、様々な方針でマッチできるようになっているのです。

# 類似のライブラリーとの比較を軽く

## 各種パーサーコンビネーター

さて、ここまでこの文章を読んでいただけた方の中には、「これってmegaparsecとかattoparsecとかのパーサーコンビネーターライブラリーと何が違うんだ？」という疑問をお持ちの方も多いでしょう。  
そう、大抵の場合、パーサーコンビネーターライブラリーも下記のような特徴を持ち合わせています。

- Haskellの内部DSLとして実装されている
    - `Applicative`や`Alternative`型クラスのメソッドを利用したAPI
- マッチした結果から（文字列以外の）Haskellの値に割り当てるのが簡単
- 「文字(`Char`)」の列以外にもマッチできる

特に「`Applicative`や`Alternative`型クラスのメソッドを利用したAPI」である点は興味深く、場合によっては、使うライブラリーだけ換えて式をコピペしてもコンパイルは通る、なんてことが普通にあり得るくらい似ています。  
ただし、当然コンパイルが通るだけでは意図通りに動くとは限りません。  
regex-alternativeと一般的なパーサーコンビネーターライブラリーには、「**自動的にバックトラックをするかしないか**」という違いがあるためです。

例えば、次の式はregex-applicativeでもattoparsecでも有効な式ですが、regex-applicativeの`match`関数では、「`ab`が1回以上繰り返される文字列」にマッチして最後の`ab`を返すことができるのに、attoparsecの`parse`関数ではパースに失敗してしまいます。

```haskell
many (string "ab") *> string "ab"
```

`stack build regex-applicative attoparsec`した上で以下のように書いて試してみましょう。  
まずはregex-applicativeで試す場合:

```haskell
> import Text.Regex.Applicative

> match (many (string "ab") *> string "ab") "abab"
Just "ab"

> match (many (string "ab") *> string "ab") "ab"
Just "ab"

> match (many (string "ab") *> string "ab") "ababab"
Just "ab"
```

いずれの文字列でも`Just "ab"`が返ってきてますね😌。

続いてattoparsecで試す場合:

```haskell
-- attoparsecは`String`をサポートしてないのでOverloadedStringsでTextとして扱う
> :set -XOverloadedStrings

> import Control.Applicative
> import Data.Attoparsec.Text

-- 文字列の終端であることを明確にするために、空文字列をfeedしておく
> feed (parse (many (string "ab") *> string "ab") "abab") ""
Fail "" [] "not enough input"

> feed (parse (many (string "ab") *> string "ab") "ab") ""
Fail "" [] "not enough input"

> feed (parse (many (string "ab") *> string "ab") "ababab") ""
Fail "" [] "not enough input"
```

いずれの文字列でも失敗になってしまいました。なぜうまくいかないのでしょう？  
それは文字列`"ababab"`における`ab`を、`many (string "ab")`が消費してしまい、`*>`の右辺に書いた`string "ab"`が処理できなくなってしまうためです。  
対するregex-applicativeにおける`many (string "ab") *> string "ab"`では、正規表現全体がマッチするよう、自動でバックトラックしてくれます。  
regex-applicativeでも最初に`many (string "ab")`が`"ababab"`全体を消費した直後では、`*>`の右辺に書いた`string "ab"`のマッチは当然失敗してしまいます。  
しかし、regex-applicativeはそれではあきらめません。`*>`の右辺に書いた`string "ab"`が成功するまで、失敗する度に`many (string "ab")`が消費した文字を1文字ずつ返却してくれるのです。これがバックトラックです。  
regex-alternativeに限らず、大抵の正規表現エンジンがこのように自動的なバックトラックを行います。

こうした性質の違いにより、regex-applicativeは**文字列の中間に指定したパターンをマッチさせる**のが、パーサーコンビネーターライブラリーよりも得意です。

例えば「文字列の中間にある1桁以上の10進数」にマッチさせる場合、regex-alternativeでは次のように書きます。

```haskell
> import Text.Regex.Applicative.Common
> match (few anySym *> decimal <* few anySym) "abc12345def"
Just 12345
```

`few`は「控えめな繰り返し」を実現するための関数です。引数で指定した正規表現を0回以上マッチさせる、という点では`many`と同じですが、前後にある正規表現がなるべく長くマッチするよう、優先してマッチさせてくれます。  
`few anySym`は普通の正規表現ライブラリーでいうところの`.*?`に相当します。

同じことをattoparsecで実現するために`many anyChar *> decimal <* many anyChar`と書いてみても、やはりうまくいきません。

```haskell
> import Data.Attoparsec.Text

> feed (parse (many anyChar *> decimal <* many anyChar) "abc12345def") ""
Fail "" [] "not enough input"
```

理由は先ほどと同様で、最初に書いた`many anyChar`がすべての文字列を消費してしまい、それ以降の`decimal`などがマッチできないためです。  
正しく処理するには、「`decimal`の先頭以外の文字列」、すなわち「数字以外の文字列」が`many`であることを明示する方法をとるしかありません[^regex]。

[^regex]: ただし、一般に、正規表現ライブラリーであってもこのような書き方をした方が効率よくマッチさせやすいでしょう。

```haskell
> import Data.Char

> nonDigits = many (satisfy (not . isDigit))
> feed ((parse (nonDigits *> decimal <* nonDigits)) "abc12345def") ""
Done "" 12345
```

そんなわけで、regex-applicativeは、Haskellによくあるパーサーコンビネーターのように**Applicativeスタイルで書けて、なおかつ他の正規表現ライブラリーのように中間マッチがしやすい**という、両方の良さを持ち合わせていると言えます。

### 番外編: replace-attoparsec・replace-megaparsec

...と、regex-applicativeのよさを語ったところで舌の根も乾かぬうちに恐縮ですが、実はattoparsecをはじめパーサーコンビネーターライブラリーの「中間マッチがやりにくい」という弱点を改善するためのパッケージがあります。  
[replace-attoparsec](http://hackage.haskell.org/package/replace-attoparsec)や[replace-megaparsec](http://hackage.haskell.org/package/replace-megaparsec)といいます[^substring-parser]。  
名前のとおりreplace-attoparsecがattoparsecを改善するパッケージで、replace-megaparsecがmegaparsecを改善するパッケージです。  
名前もAPIもお互いそっくりなんで<small>（作者も同じですしね）</small>、今回はreplace-attoparsecの方を紹介しましょう。

[^substring-parser]: [こちらの記事](https://haskell.jp/blog/posts/2018/substring-parser.html)でも触れているとおり、かつて私も同じ目的のパッケージを作成しました。しかし、これらのパッケージの方が明らかにドキュメントが充実していて、機能も豊富なので今回はこれらを紹介します。将来的にはsubstring-parserはdeprecatedにするかも知れません。

replace-attoparsecを使えば、次のように書くだけで「文字列の中間にある1桁以上の10進数」を取り出すことができます。

```hakell
import Replace.Attoparsec.Text

> feed (parse (sepCap decimal) "abc12345def") ""
Done "" [Left "abc",Right 12345,Left "def"]
```

`"abc12345def"`の中間にある`12345`だけでなく、パースできなかった`abc`、`def`という文字列もおまけで取得できました！  
`decimal`がパースできた箇所が`Right`として、パースできなかった箇所が`Left`として返却されていることに注意してください。

replace-attoparsecの`sepCap`<small>（「Separate and Capture」の略だそうです）</small>は、引数として受け取ったパーサーを、

1. とりあえず先頭からマッチさせてみて、
1. 失敗したら先頭の一文字をスキップして、次の文字からまたマッチさせてみる

という処理を繰り返しています。  
結果的にパースできない文字列はすべてスキップして、文字列の中間にある、パースできる文字列のみにパーサーを適用できるのです。

## VerbalExpressions

そろそろ力尽きてきたのでここからはスライドのコピペで失礼します...🙏

- 詳細わかりませんが作りはよく似てる
    - [JavaScriptの例がこちら](https://github.com/VerbalExpressions/JSVerbalExpressions#examples)
- 変な記号の演算子ではなく英語でつけられた関数なので、こちらの方が分かりやすいという人は多そう
- [Haskellを含むいろんな言語で提供されてるらしい](https://github.com/VerbalExpressions)
- さっと[Haskell版のドキュメント](http://hackage.haskell.org/package/verbalexpressions-1.0.0.0/docs/Text-Regex-VerbalExpressions.html)読んだ感じ、文字列のマッチに特化してるっぽい？

# まとめ

以上です！👋  
まとめもスライドからのコピペで！

- regex-applicativeは、Haskellの式で正規表現を書ける内部DSL
- パーサーコンビネーターっぽく使えて、かつ正規表現の良さを持ち合わせている
- 内部は「文字を受け取って続きの状態のリストを返す関数」として表現されたNFAで実装されている
