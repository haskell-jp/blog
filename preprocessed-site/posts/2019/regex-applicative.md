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

<small>（最近の）</small>`cabal`の場合は`cabal.project`があるディレクトリーで👇を実行すればできるはずです。

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

## 空文字（ε）: `pure` :: a -> RE s a

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

`<*`を使うことで、`://`の部分にはマッチさせてもマッチした結果は無視している点にご注意ください。  
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

[^https]: もちろん、実際のところhttpsの場合デフォルトのポート番号は443ですが、ちゃんと実装しようとすると結構複雑になるのでご容赦を！

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

regex-applicativeを使うことで、URLのオリジンにマッチさせるだけでなく、待ちした結果を`Origin`型の値として割り当てる正規表現が作れました！🎉

# 👍regex-applicativeのメリット

- 内部DSLとして書けるので、コンパイラーによる型チェックの恩恵を受けやすい
- 文字列以外の扱いにも強い
    - マッチした結果から（文字列以外の）Haskellの値に割り当てるのが簡単！
        - 「生のデータ」から「コアの処理が欲しいデータ」への変換がワンストップ
    - 文字列だけでなく、任意のリストに対してマッチできる

# 👎regex-applicativeのデメリット

- コードは長い
- 速度はおそらくCとかで書いたものほど速くはない
    - そんなに細かい最適化をしているわけではないし、Pure Haskellなので...
- Haskellの`String`以外の文字列にはマッチできない...
    - 参考: [Haskell Tips (文字列編) - りんごがでている](http://bicycle1885.hatenablog.com/entry/2012/12/24/234707)

# ⚙️regex-applicativeの仕組み

- 継続渡しで作られたNFA
- バックトラックするときは継続を切り替える

# 📑正規表現エンジンの分類

※[正規表現技術入門](https://gihyo.jp/book/2015/978-4-7741-7270-5) p. 56より

- DFA型
    - 正規表現を決定性有限オートマトン（deterministic finite automaton）と呼ばれるものに変換して正規表現マッチングを行う
- VM型
    - 正規表現をバイトコード（bytecode）と呼ばれるものに変換して正規表現マッチングを行う

# regex-applicativeの場合は...？

😕どちらでもなさそう？

- NFAをDFAに変えずにそのまま使っている
- NFAにおける状態遷移を「文字を受け取って次の状態のリストを返す関数」で表す

# regex-applicativeの実際の実装

- [`compile`](https://github.com/feuerbach/regex-applicative/blob/5e9a06622d33c7657353ddaccfe101b96946027a/Text/Regex/Applicative/Object.hs#L110-L111)という関数で、正規表現オブジェクト`RE s a`を[`ReObject`](https://github.com/feuerbach/regex-applicative/blob/5e9a06622d33c7657353ddaccfe101b96946027a/Text/Regex/Applicative/Object.hs#L38-L43)という、[`Thread`](https://github.com/feuerbach/regex-applicative/blob/5e9a06622d33c7657353ddaccfe101b96946027a/Text/Regex/Applicative/Types.hs#L9-L16)オブジェクトのキューに変換する
- `Thread`は👇の二通りの値をとりうる型
    - ⏩`s -> [Thread s r]`: 文字を受け取って次に分岐しうる`Thread`のリストを返す関数。文字が条件にマッチしなければ空リストを返して次に進まない
    - ✅`Accept r`: 文字どおり受理状態を表す
    - ⚠️並行並列プログラミングで出てくるあの「スレッド」とは違うので注意！

# regex-applicativeの実際の実装（続き）

- ➡️ `s -> [Thread s r]`の実行が成功したとき
    - 返却された`[Thread s r]`の要素をすべてキューに追加して、引き続き実行する
- ↩️ `s -> [Thread s r]`の実行が失敗したとき
    - バックトラック: 次に実行する`Thread`に切り替える
- ℹ️ 実際には`Thread`を一つずつ実行してみて、結果が条件に合うものを選ぶ、といった方が近い
    - `findLongestPrefix`関数などはそうした処理特性によって実現

# 類似のライブラリーとの比較を軽く

- 各種パーサーコンビネーター
- VerbalExpressions

## 各種パーサーコンビネーター

- Haskell向けのパーサーコンビネーターの多くは`Applicative`ベースのAPIなので、ぱっと見よく似てる
    - 場合によっては使うライブラリーだけ換えて式をコピペしてもコンパイルは通る（かも）
- バックトラックするかしないか
    - hoge: コード例で示そう
- regex-applicativeの方が部分文字列へのマッチが簡単
    - パーサーコンビネーターを、部分文字列のマッチに使いやすくするライブラリーもあるにはある
        - [replace-attoparsec](https://github.com/jamesdbrock/replace-attoparsec)

## VerbalExpressions

- 詳細わかりませんが作りはよく似てる
    - [JavaScriptの例がこちら](https://github.com/VerbalExpressions/JSVerbalExpressions#examples)
- 変な記号の演算子ではなく英語でつけられた関数なので、こちらの方が分かりやすいという人は多そう
- [Haskellを含むいろんな言語で提供されてるらしい](https://github.com/VerbalExpressions)
- さっと[Haskell版のドキュメント](http://hackage.haskell.org/package/verbalexpressions-1.0.0.0/docs/Text-Regex-VerbalExpressions.html)読んだ感じ、文字列のマッチに特化してるっぽい？

# まとめ

- regex-applicativeは、Haskellの式で正規表現を書ける内部DSL
- パーサーコンビネーターっぽく使えて、かつ正規表現の良さを持ち合わせている
- 内部は「文字を受け取って続きの状態のリストを返す関数」として表現されたNFAで実装されている
