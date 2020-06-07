---
title: Strict拡張を使用する際の注意点
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: YAMAMOTO Yuji
postedBy: <a href="http://the.igreque.info/">YAMAMOTO Yuji(@igrep)</a>
date: January 26, 2020
tags:
...
---


Haskellは他の多くのプログラミング言語と異なった特徴を備えており、しばしばそれらが議論を呼ぶことがあります。  
その中でも特によく俎上に上がるのが、遅延評価です。  
遅延評価は、適切に扱えば不要な計算を行わず、計算資源を節約してくれるステキな仕組みですが、一歩使い方を間違うと「サンク」という「これから実行する<small>（かも知れない）</small>計算」を表すオブジェクトが無駄に作られてしまい、却ってメモリー消費量が増えてしまう、などといった問題を抱えています。  
この現象は「スペースリーク」と呼ばれ、かつて[専門のAdvent Calendar](https://qiita.com/advent-calendar/2015/haskell-space-leaks)が作られたことがあるほど、Haskeller達の関心を集めてきました。

そんなHaskeller達の悩みの種を軽減しようと、GHC 8.0以降、[`Strict`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#strict-by-default-pattern-bindings)と[`StrictData`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#strict-by-default-pattern-bindings)という言語拡張が搭載されました。  
これらの拡張は、大雑把に言うと、

- `StrictData`: 値コンストラクターにおいて、引数の値が弱頭正規形（Weak Head Normal Form。以降慣習に従い「WHNF」と呼びます）まで評価されるようになる
- `Strict`: `StrictData`の効果に加え、あらゆる関数やローカル変数の定義において、パターンマッチで代入した変数の値がWHNFまで評価されるようになる

というものです。

このうち、`StrictData`は比較的リスクが少なく大変有用<small>（もはや標準であって欲しいぐらい）</small>という声をよく聞きますが[^strictdata-sample]、`Strict`については様々な問題点があることが知られています。  
今回はその各種問題点をまとめて共有することで、思い切って`Strict`を有効にするときに参考になる情報を提供したいと思います！

[^strictdata-sample]: 例えばfumievalさんによる[この記事](http://fumieval.hatenablog.com/entry/2015/12/10/200630)より:  
> もっとも、日常ではここまで気にしなければいけない場面は少ないので、ほとんどの場合は気にせず感嘆符をつけて大丈夫だろう。GHC 8.0からは、全フィールドをデフォルトで正格にする`StrictData`という拡張が入るため、こちらを使おう。

# 前提知識とその参考資料

以下の知識について、ざっくり理解しているものとして進めます。  
参考になりそうな日本語のページも付記したので、ご覧ください。

- Haskellの遅延評価について
    - [実装して理解する遅延評価の仕組み 〜 thunkを絵に描いて理解しよう・JavaScriptでHaskellを実装！？ - プログラムモグモグ](https://itchyny.hatenablog.com/entry/20130209/1360417348)が詳しくて分かりやすいでしょう
- Haskellの正格評価について
    - [正格性のすべて (翻訳)](https://haskell.e-bigmoon.com/posts/2018/06-25-all-about-strictness)
- `Strict`と`StrictData`について
    - [Strict Haskell - あどけない話](https://kazu-yamamoto.hatenablog.jp/entry/20151117/1447726679)
- その他、[Haskellスペースリーク Advent Calendar 2015 - Qiita](https://qiita.com/advent-calendar/2015/haskell-space-leaks)の記事にも有用なものがたくさんあります。

# サンプルコードの試し方

これから紹介するコードは、すべて[このブログのリポジトリーの、`examples`ディレクトリー](https://github.com/haskell-jp/blog/tree/master/examples/2020/strict-gotchas)に置いておきました。  
下記のコマンドを実行すれば実際に試すことができます<small>（一部実行する際のコマンドが異なりますので、適宜例示します）</small>。

```
git clone https://github.com/haskell-jp/blog.git
cd blog/examples/2020/strict-gotchas
stack exec runghc -- <これから紹介するコードのファイル>.hs
```

実際に試すときは`-XStrict`というオプションを`runghc`に付けた場合と付けなかった場合両方で実行して、違いを確かめてみてください。

なお、使用したGHCのバージョンは8.10.1で、OSはWindows 10 ver. 1909です。

# Case 1: `where`句だろうとなんだろうと評価

サンプル: [where.hs](https://github.com/haskell-jp/blog/blob/master/examples/2020/strict-gotchas/where.hs)

最初のケースは、遅延評価で当たり前に享受できていたメリットが、`Strict`を有効にしている状態では得られなくなってしまう、というものです。  
[pfxfncさんのStrict拡張でハマったお話](https://qiita.com/pxfnc/items/a26bda6d11402daba675)という記事でも紹介されてはいますが、まとめ記事なのでここでも改めて取り上げます。

```haskell
main = print $ div10 0

div10 :: Int -> Int
div10 n
  | n == 0    = 0
  | otherwise = result
 where
  result = 10 `div` n
```

ご覧のとおり、本当にほとんどpfxfncさんの記事のサンプルそのままで恐縮ですが、このプログラム、👇のように`Strict`拡張を有効にして実行するとエラーが起こります。

```bash
> stack exec -- runghc --ghc-arg=-XStrict where.hs
where.hs: divide by zero
```

一方、`Strict`拡張を有効にしなかった場合、エラーは起こりません。

```bash
> stack exec -- runghc where.hs
0
```

なぜこんなことが起こるのでしょう？

これは、`Strict`拡張がパターンマッチで代入したあらゆる変数の値をWHNFまで評価するようになった結果、`where`句で代入した変数まで必ずWHNFまで評価してしまうために発生したエラーです。  
すなわち、`where`における、

```haskell
  result = 10 `div` n
```

までもが、

```haskell
  !result = 10 `div` n
```

とBangパターンを付けた代入であるかのように解釈されたのです[^bangpatterns]。

[^bangpatterns]: `BangPatterns`言語拡張を有効にした上で上記のように書き換えてみると、`Strict`拡張の有無に関わらずエラーが発生します。試してみましょう。

こうなると、`result`を使用しないケース、すなわち`n == 0    = 0`の場合であっても`result`に <small>（WHNFまで評価した）</small>値を代入するのに必要な計算は実行され、結果<code>10 `div` 0</code>が計算されようとして`divide by zero`が発生するのです。

⚠️`where`句は関数定義の後ろの方に書くという性格上、見落としがちかも知れません。注意しましょう。

# Case 2: ポイントフリースタイルかどうかで変わる！

サンプル: [const.hs](https://github.com/haskell-jp/blog/blob/master/examples/2020/strict-gotchas/const.hs)

続いて、Haskellに慣れた方なら誰もが一度は試したくなる、ポイントフリースタイルに関する落とし穴です。  
まずは次の二つの関数をご覧ください。

```haskell
dontReferArgs :: a -> b -> a
dontReferArgs = const

referArgs :: a -> b -> a
referArgs x _ = x
```

この関数、どちらもやっていることは`const`と変わりません。  
`dontReferArgs`は`const`をそのまま使うことでポイントフリースタイルにしていますが、`referArgs`は自前で引数に言及することで`const`と同等の定義となっています。  
ポイントフリースタイルに変えると言うことは原則として元の関数の挙動を変えないワケですから、`dontReferArgs`と`referArgs`の意味は変わらないはず、ですよね[^opt]？

[^opt]: 実際のところ今回紹介するケース以外にも、ポイントフリースタイルにするかしないかで実行効率などが変わる場合があります。例えば、[Evaluation of function calls in Haskell](https://treszkai.github.io/2019/07/13/haskell-eval)をご覧ください。

ところがこれらの関数を`Strict`拡張を有効にした上で定義すると、なんと挙動が異なってしまいます！

使用例:

```haskell
main :: IO ()
main = do
  print $ dontReferArgs "dontReferArgs" (undefined :: Int)
  print $ referArgs "referArgs" (undefined :: Int)
```

実行結果（Strict拡張を有効にしなかった場合）:

```bash
> stack exec runghc const.hs
"dontReferArgs"
"referArgs"
```

実行結果（Strict拡張を有効にした場合）:

```bash
> stack exec -- runghc --ghc-arg=-XStrict const.hs
"dontReferArgs"
const.hs: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries\base\GHC\Err.hs:79:14 in base:GHC.Err
  undefined, called at const.hs:10:34 in main:Main
```

はい、`where`句のケースと同様、`Strict`拡張を有効にした場合、例外が発生してしまいました❗️  
`Strict`拡張を有効にした結果、意図せず例外を発生させる値<small>（今回の場合`undefined`）</small>が評価されてしまったのです。

例外を発生させた関数はそう、ポイントフリースタイルでない、`referArgs`関数の方です！  
なぜ`referArgs`でのみ例外が発生してしまったのかというと、`referArgs`が`Strict`拡張を有効にしたモジュールで、引数に言及<small>（パターンマッチ）</small>しているからです。  
`Strict`拡張を有効にした結果「あらゆる関数やローカル変数の定義において、パターンマッチで代入した変数の値」が評価されるとおり、`referArgs`の引数`x`・`_`も必ず評価されるようになり、このような例外が発生したのです。  
たとえ使用しない変数`_`でも関係ありません！

そのため、原因の本質は引数に言及<small>（パターンマッチ）</small>しているか否かであり、`Prelude`の`const`を使用しているか否かではありません。  
こちら👇のように引数に言及した上で`const`を使っても、結果は同じなのです。

```haskell
referArgsByConst :: a -> b -> a
referArgsByConst x y = const x y
```

```haskell
print $ referArgsByConst "referArgsByConst" (undefined :: Int)
```

一方、`dontReferArgs`については、引数に言及せず、`Prelude`にある`const`をそのまま使っています。  
`Strict`拡張はあくまでも「パターンマッチした変数」のみをWHNFまで評価するものであり、あらゆる関数が正格に呼び出されるわけではありません。  
なので通常の`Prelude`における`const`と同様、`dontReferArgs`も第2引数は評価しないため、`undefined`を渡しても例外は起こらなかったのです。

このことは、「**`Strict`拡張を有効にしているモジュールの中でも、`Strict`を有効にしていないモジュール<small>（この場合は`Prelude`）</small>から`import`した関数は、引数を正格に評価しない**」という忘れてはならないポイントも示しています。  
例えば`const`よりももっと頻繁に使われるであろう、言及する引数を一つ削除する演算子の代表、関数合成`.`を使ったケースを考えてみてください。

ポイントフリースタイルに慣れた方なら、関数適用`$`を次👇のように使って定義した`f`を見ると、

```haskell
f xs = map (+ 3) $ filter (> 2) xs

-- あるいは、`$`を使わないでこのように書いた場合も:
f xs = map (+ 3) (filter (> 2) xs)
```

こちら👇のように書き換えたくなってうずうずするでしょう。

```haskell
f = map (+ 3) . filter (> 2)
```

しかし、`Strict`を有効にしたモジュールでこのような書き換えを行うと、`f`の挙動が変わってしまいます。  
引数`.`を使って書き換える前は、引数`xs`に言及していたところ`.`を使って引数`xs`に言及しなくなったからです。  
`filter`も`map`も`Strict`拡張を有効にしたモジュールで定義されているわけではないので、引数を正格に評価しないんですね。  
結果、こうした書き換えによって、**`Strict`拡張を有効にしていても意図せず遅延評価してしまう**、というリスクがあるので、リファクタリングの際はくれぐれも気をつけてください[^list]。  
ざっくりまとめると、`Strict`拡張を有効にしているモジュールでは、「引数や変数を宣言することすなわちWHNFまで評価すること」、あるいは「引数や変数を宣言しなければ、評価されない」と意識しましょう。

[^list]: もっとも、この場合引数はリストでしょうから、WHNFまでのみ正格評価するメリットは少なそうですが。

ちなみに、`referArgs`における`_`のように「`Strict`拡張を有効にした場合さえ、使用していない引数が評価されてしまうのは困る！」という場合は、引数名の前にチルダ`~`を付けてください。

```haskell
referArgs :: a -> b -> a
referArgs x ~_ = x
```

# Case 3: 内側のパターンはやっぱりダメ

サンプル: [tuple.hs](https://github.com/haskell-jp/blog/blob/master/examples/2020/strict-gotchas/tuple.hs)

続いては、`Strict`拡張のドキュメントでも触れられている、入れ子になったパターンマッチにおける問題を紹介します。  
一言で言うと、`let (a, b) = ...`のような、データ構造<small>（この場合タプルですね）</small>の「内側」に対するパターンマッチは、`Strict`拡張を有効にしていても正格に評価しないよ、という話です。

例えば、下記のコードを`Strict`拡張付きで実行しても、パターンマッチしている`a`・`b`ともに代入した時点では正格評価されず、`error "a"`・`error "b"`による例外はいずれも発生しません。

```haskell
(a, b) = (error "a", error "b")
```

これは、タプルの値コンストラクター`(,)`が<small>（`StrictData`やStrictness flagを用いないで定義されているので）</small>各要素を遅延評価することとは**関係なく**、各要素を正格評価する値コンストラクターであっても同様です。

```haskell
data MyTuple a b = MyTuple !a !b deriving Show

MyTuple a b = MyTuple (error "a") (error "b")
```

先ほどの節における「`Strict`拡張を有効にしているモジュールでは、『引数や変数を宣言することすなわちWHNFまで評価すること」』、あるいは『引数や変数を宣言しなければ、評価されない』と意識しましょう」という主張を真に受けてしまうと意図せず遅延評価させてしまい、ハマりそうです😰。  
⚠️繰り返しますが「**内側のパターンにおける変数は正格評価されない**」ということも意識してください。

以上のことを試していただくために、サンプルコードでは`Strict`を有効にしてもしなくても、結果が変わらないサンプルを用意しました<small>（コードは👆の例に`main`がくっついたのとあまり変わらないので解説は省きます）</small>。  
以下、実行例を。

```bash
> stack exec runhaskell ./tuple.hs
"Default tuple"
"Default tuple in MyTuple1"
"Other value in MyTuple1"

> stack exec -- runghc --ghc-arg=-XStrict ./tuple.hs
"Default tuple"
"Default tuple in MyTuple1"
"Other value in MyTuple1"
```

# Case 4: `foldr`に渡す関数

サンプル: [stackoverflow-foldr.hs](https://github.com/haskell-jp/blog/blob/master/examples/2020/strict-gotchas/stackoverflow-foldr.hs)

ここの話はちょっと難しいので、先に守るべきルールを述べておきます。

「遅延評価する関数を受け取る前提の高階関数に、（`Strict`拡張などで）引数を正格に評価するよう定義された関数を渡すのは止めましょう。」

なんだかこう書くと半ばトートロジーのようにも聞こえますが、より具体的には、例えば`foldr`に引数を正格に評価するよう定義された関数を渡すのは止めましょう、という話です。  
`Strict`拡張を有効にした状態では、ラムダ式にも注意しないといけないもポイントです。

※あらかじめおことわり: この節のお話は、あくまでもリストに対する`foldr`の場合のお話です。  
他の`Foldable`な型では必ずしも当てはまらないのでご注意ください。

論より証拠で、サンプルコードの中身（抜粋）とその実行結果を見てみましょう。

```main
-- ...
evaluate . length $ foldr (:) [] [1 .. size]
putStrLn "DONE: foldr 1"

evaluate . length $ foldr (\x z -> x : z) [] [1 .. size]
putStrLn "DONE: foldr 2"
```

今回のサンプルコードを実行する際は、GHCのランタイムオプションを設定して、スタック領域のサイズを減らしてください。  
そうでなければ、処理するリストがあまり大きくないので`Strict`拡張を有効にしても問題の現象は再現されないでしょう[^bigger-list]。  
[こちらのStackoverflowの質問](https://stackoverflow.com/questions/29339643/how-can-i-pass-rts-options-to-runghc)曰く、`runghc`で実行する際にランタイムオプションを設定する場合は、`GHCRTS`環境変数を使用するしかないそうです。

[^bigger-list]: 大きなリストにすると、今度はエラーが発生するまでに時間がかかってしまうので...。  
ちなみに、このようにスタック領域を小さくすることでスペースリークを検出する手法は、[ndmitchell/spaceleak: Notes on space leaks](https://github.com/ndmitchell/spaceleak)でも紹介されています。

実行結果（Strict拡張を有効にしなかった場合）:

```bash
> GHCRTS=-K100k stack exec runghc -- ./stackoverflow-foldr.hs
DONE: foldr 1
DONE: foldr 2
```

実行結果（Strict拡張を有効にした場合）:

```bash
> GHCRTS=-K100k stack exec runghc -- --ghc-arg=-XStrict ./stackoverflow-foldr.hs
DONE: foldr 1
stackoverflow-foldr.hs: stack overflow
```

はい、サンプルコードは整数のリストに対して特に何も変換せず`foldr`する<small>（そして、`length`関数でリスト全体を評価してから捨てる）</small>だけのことを2回繰り返したコードです。  
最初の`foldr`は`Strict`拡張があろうとなかろうと無事実行できたにもかかわらず、`Strict`拡張を有効にした二つめの`foldr`は、`stack overflow`というエラーを起こしてしまいました💥！

なぜこんなエラーが発生したのかを知るために、`foldr`の定義を見直しましょう。  
こちら👇は[GHC 8.10.1における、リストに対する`foldr`の定義](http://hackage.haskell.org/package/base-4.14.0.0/docs/src/GHC.Base.html#foldr)です<small>（コメントは省略しています）</small>。

```haskell
foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys
```

`go`という補助関数を再帰的に呼び出すことで、第一引数として渡した関数`k`を用いてリストの要素(`y`)を一つずつ変換しています。  
呼び出す度にリストの残りの要素をチェックして、最終的に空のリストを受け取ったときは`foldr`の第二引数`z`を返していますね。

このとき`k`が第二引数を遅延評価する関数であった場合、 --- サンプルコードで言えば`(:)`の場合 --- 受け取った`go ys`という式は直ちには評価されません。  
サンプルコードの`(:)`に置き換えると、`(:)`の第二引数、つまりリストの残りの要素を取り出す度に`go ys`を一回計算して、一個ずつ要素を作り出すイメージです。

一方、`k`が第二引数を正格評価する関数であった場合、 --- サンプルコードで言うところの、`Strict`拡張を有効にした`(\x z -> x : z)`の場合 --- `k`は受け取った`go ys`をすぐに評価しようとします。  
このとき、GHCは`k`や`go`に渡されている引数をスタック領域に積みます[^rts]。  
そうして`go`と、`go`に呼ばれた`k`が次々と引数をスタック領域に積んだ結果、スタックサイズの上限に達し、スタックオーバーフローが発生してしまうのです。

[^rts]: GHCがどのように評価し、スタック領域を消費するかは[GHC illustrated](https://takenobu-hs.github.io/downloads/haskell_ghc_illustrated.pdf)や、その参考文献をご覧ください。

これは他の多くのプログラミング言語で<small>（末尾再帰じゃない、普通の）</small>再帰呼び出しを行った場合とよく似た振る舞いです。  
間違って無限再帰呼び出しをしてしまってスタック領域があふれる、なんて経験は多くのプログラマーがお持ちでしょう。  
つまり単純に、`Strict`拡張を有効にした場合の`foldr (\x z -> x : z) []`は、再帰呼び出しをしすぎてしまう関数となるのです。

なお、今回は`length`関数を使ってリスト全体を使用するコードにしましたが、遅延リストらしく`foldr`の結果を一部しか使わない、という場合、`foldr`に渡した関数がリストを都度正格評価してしまうので、無駄な評価が占める割合はもっと増えることになります。  
やはり`foldr`は遅延評価を前提とした高階関数と言えるでしょう。

以上のとおり、Haskellには`foldr`のような、遅延評価を前提とした関数が`Strict`拡張より遥か昔から存在しています。  
それらを`Strict`拡張を有効にした状態で使うと、思わぬ衝突が起きてしまうので、くれぐれも気をつけましょう。

こういう「使ってはいけない関数」を引いてしまわないための方法についても一点補足します。  
HLintを細かく設定したり、カスタム`Prelude`を設定したりしてみるのは、一つの作戦です。なんとプロジェクト全体で、`foldr`を禁止することができます<small>（一部のモジュールでは例外的に許可する、なんてこともできます）</small>。  
詳しくは[「素晴らしき HLint を使いこなす」](https://haskell.e-bigmoon.com/posts/2018/01-29-awesome-hlint.html)や[「Prelude を カスタムPrelude で置き換える」](https://haskell.e-bigmoon.com/posts/2018/05-23-extended-prelude.html)をご覧ください。

# Case 5: `undefined`を受け取るメソッド

サンプル: [storable.hs](https://github.com/haskell-jp/blog/blob/master/examples/2020/strict-gotchas/storable.hs)

最後はちょっとレアケースではありますが、こちら👇のIssueで発覚した問題を解説しましょう。

[#16810: Use explicit lazy binding around undefined in inlinable functions · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/issues/16810)

問題を簡単に再現するために、次のサンプルコードを用意しました。

```haskell
-- importなどは当然省略！
data Test = Test Int Int deriving Show

instance Storable Test where
  sizeOf _ = sizeOf (1 :: Int) * 2
  alignment _ = 8
  peek = error "This should not be called in this program"
  poke = error "This should not be called in this program"

main = alloca $ \(_ :: Ptr Test) -> putStrLn "This won't be printed when Strict is enabled"
```

はい、適当な型を定義して[`Storable`](https://downloads.haskell.org/~ghc/8.10.1/docs/html/libraries/base-4.14.0.0/Foreign-Storable.html#t:Storable)のインスタンスにして、それに対して[`alloca`](https://downloads.haskell.org/~ghc/8.10.1/docs/html/libraries/base-4.14.0.0/Foreign-Marshal-Alloc.html#v:alloca)を呼ぶだけのコードです。  
インスタンス定義をはじめかなり手抜きな感じになっちゃってますが、まぁ今回の問題を再現するのにはこれで十分なので、ご了承ください🙏。

このコード、残念ながら`Strict`拡張を有効にした状態で実行すると、`undefined`による例外が発生してしまいます💥。

```bash
> stack exec -- runghc --ghc-arg=-XStrict storable.hs
storable.hs: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries\base\GHC\Err.hs:79:14 in base:GHC.Err
  undefined, called at libraries\base\Foreign\Marshal\Alloc.hs:117:31 in base:Foreign.Marshal.Alloc
```

こちらは`Strict`を有効にしなかった場合。やはり例外は起きてませんね😌。

```bash
> stack exec -- runghc storable.hs
This won't be printed when Strict is enabled
```

さてこの、`Strict`拡張を有効にした場合に発生した、`undefined`による例外はどこからやってきたのでしょう？  
上記のコードにはいくつか`error`関数を使用している箇所がありますが、発生した例外はあくまでも`undefined`です。見た限り上記のコードそのものから発生した例外ではなさそうですね...🤔。

その答えはなんと、`main`関数で呼んでいる[`alloca`の定義](https://downloads.haskell.org/~ghc/8.10.1/docs/html/libraries/base-4.14.0.0/src/Foreign-Marshal-Alloc.html#alloca)にありました！

```haskell
alloca :: forall a b . Storable a => (Ptr a -> IO b) -> IO b
alloca  =
  allocaBytesAligned (sizeOf (undefined :: a)) (alignment (undefined :: a))
```

確かに、`sizeOf`メソッドや`alignment`メソッドに`undefined`を渡しています。  
これらはいずれも`Storable`型クラスのメソッドなので、上記の`Test`型でももちろん実装しています。  
そう、実はこの`sizeOf`メソッドと`alignment`メソッドの実装で、下👇のように引数`_`を宣言しているのが問題なのです！

```haskell
instance Storable Test where
  sizeOf _ = sizeOf (1 :: Int) * 2
  alignment _ = 8
  -- ...
```

[「Case 2: ポイントフリースタイルかどうかで変わる！」の節](#TODO)で、「`Strict`拡張を有効にしているモジュールでは、『引数や変数を宣言することすなわちWHNFまで評価すること」』、あるいは『引数や変数を宣言しなければ、評価されない』」と述べたことを再び思い出してください。  
こちらの`sizeOf`・`alignment`の定義でも同様に、引数`_`を宣言しているため、引数を必ずWHNFまで評価することになっています。  
結果、`alloca`関数がそれぞれを呼ぶ際`undefined`を渡しているため、`undefined`を評価してしまい、`undefined`による例外が発生してしまうのです💥。

なぜこのように、`alloca`関数では`sizeOf`や`alignment`に`undefined`をわざわざ渡しているのでしょう？  
それは、これらのメソッドがそもそも`undefined`を渡して使うことを前提に設計されているからです。  
`sizeOf`・`alignment`はともに`Storable a => a -> Int`という型の関数なので、第一引数に`Storable`のインスタンスである型`a`の値を受け取るのですが、このとき**渡される`a`型の値は、使わない**こととなっています。  
[それぞれのメソッドの説明](https://downloads.haskell.org/~ghc/8.10.1/docs/html/libraries/base-4.14.0.0/Foreign-Storable.html#v:sizeOf)にも「The value of the argument is not used.」と書かれていますね。  
これは、`sizeOf`も`alignment`も、型毎に一意な値として定まる<small>（引数の値によって`sizeOf`や`alignment`の結果が変わることがない）</small>ので、第一引数の`a`は、単に「この型の`sizeOf`を呼んでくださいね」という**型の**情報を渡すためのものでしかないからです。  
だから値には関心がないので`undefined`を渡しているわけです。そもそも、`alloca`関数のように引数として`Storable a => a`型の値をとらない関数では、`a`型の値を用意することはできませんし。

現代では通常、このように「値に関心がなく、何の型であるかという情報だけを受け取りたい」という場合は、[`Proxy`](https://downloads.haskell.org/~ghc/8.10.1/docs/html/libraries/base-4.14.0.0/Data-Proxy.html#t:Proxy)型を使うのが一般的です。  
`Storable`は恐らく`Proxy`が発明される前に生まれたため、`undefined`を渡すことになってしまっているのでしょう。なので、`Storable`型クラスのインスタンスを自前で定義したりしない限り、こうしたケースに出遭うことはまれだと思います。  
ただ、それでも`Proxy`を`import`するのを面倒くさがって`undefined`を代わりに渡す、なんてケースはありえるので、`Proxy`を使って定義した型クラスでも同じ問題にハマることはあるかも知れません...。

⚠️結論として、`Storable`型クラスや、`Proxy`を受け取るメソッドを持つ型クラスのインスタンスを、`Strict`拡張を有効にした状態で定義する場合は、`Proxy`にあたる引数を評価しないよう、`~_`などを使って定義しましょう。

# おわりに: やっぱり`Strict`は使う？使わない？

さて、ここまで`Strict`拡張を有効にすることによって犯しうる、数々のミスを紹介してきました。  
ここまで書いた個人的な印象としては、「敢えて有効にする必要はないんじゃないか」といったところです。  
`foldr`の例でも触れたとおり、Haskellには遅延評価を前提とした、遅延評価を存分に活かした機能が溢れています。  
当然それらは`Strict`拡張ができるよりはるか昔からあり、`Strict`拡張のことなど一切考えないで作られたものです。  
動的型付け言語に後から静的型検査を導入するのが大変なように、相対する機能を後付けすると衝突が起こるのは仕方のないことですが、こと`Strict`拡張については想像以上に大きな衝突のようです😞。

それでも使いたいという方に、今回の記事が助けになれば幸いです💪  
それでは`Strict`な方も`NoStrict`な方もHappy Haskell Hacking!!
