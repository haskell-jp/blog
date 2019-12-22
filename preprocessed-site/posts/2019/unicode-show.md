---
title: 日本語をshowしてうまく表示されなかったら
subHeading: unicode-showの紹介（と、pretty-simpleを少し）
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: YAMAMOTO Yuji
postedBy: <a href="http://the.igreque.info/">YAMAMOTO Yuji(@igrep)</a>
date: December 22, 2019
tags: 日本語
...
---

# ℹ️この記事は🎄

この記事は、[Haskell Advent Calendar 2019](https://qiita.com/advent-calendar/2019/haskell) 22日目の記事です。  
例年どおりタイプセーフプリキュア！の話をするつもりでしたが、ネタが実装できなかったので[unicode-show](http://hackage.haskell.org/package/unicode-show)の話をします[^precure]。  
まぁ、こちらの方がみなさんにとっては有益でしょうし🙃

[^precure]: 例年どおりですとプリキュアAdvent Calendarと同時投稿をしている予定でしたが、例年参加者が減っていたこともあり、今年はプリキュアAdvent Calendarはなくなってしまいました😞

# 日本語（などの）話者がHaskellを始めるとあるある

GHCiに日本語を入力したり...

```haskell
ghci> "みんなで幸せゲットだよ！"
"\12415\12435\12394\12391\24184\12379\12466\12483\12488\12384\12424\65281"
```

日本語を`print`したり...

```haskell
ghci> print "私、堪忍袋の緒が切れました！"
"\31169\12289\22570\24525\34955\12398\32210\12364\20999\12428\12414\12375\12383\65281"
```

日本語を`show`したり...

```haskell
ghci> iimashita x = "今、" ++ show x ++ "って言いました！？"
ghci> putStrLn (iimashita "ハスケル")
今、"\12495\12473\12465\12523"って言いました！？
```

すると、日本語の大半が変な文字列に変わってしまいました😥。

へ... 変な文字列じゃないし！エスケープシーケンスに変換しただけだから！

これは、Haskell標準における`show`関数の残念な仕様です。  
`show`関数に文字列を渡すと、ダブルクォートで囲った上で、ASCII範囲外の文字列や、ASCIIの非表示文字などをエスケープシーケンスに変換して返します。  
これは、`show`関数をデバッグで使用した際、指定した文字列にどんな文字が含まれているか、簡単にわかるようにするための仕様です。  
文字の文字コードを表示すれば、NULL文字や制御文字、ゼロ幅文字、特殊なスペースなど、視認しにくいおかしな文字が含まれていても、一目でわかるのです。

しかしこれは日本語話者である我々にとって、少なくとも日本語の文字に関しては「余計なお世話」です。  
NULL文字やASCIIの制御文字といった本来画面に表示することがない文字列ならともかく、ASCII範囲外の文字列すべてをエスケープしてしまうのはやり過ぎでしょう。  
現代はUnicodeがあるおかげで、日本語に限らずともASCII範囲外の文字を扱うのは当たり前になりましたから。

# 🌐unicode-showを使おう

そこで便利なのが[unicode-show](http://hackage.haskell.org/package/unicode-show)です。  
unicode-showの`ushow`関数は、`show`がエスケープシーケンスに変換した日本語などの文字列を、元の文字列に戻してくれます。  
なので、新しい型クラスを定義する必要もなく、そのまま`Show`型クラスのインスタンスを再利用できるのです。

早速先ほどの`show`を使った例に適用してみましょう。

まずは👇のコマンドでインストールして、GHCiを起動します。

```bash
stack build unicode-show
stack exec ghci

# あるいは、最近のcabalを使っている場合は...
cabal v2-install --lib unicode-show
cabal v2-repl -b unicode-show
```

`Text.Show.Unicode`モジュールを`import`して`show`を使っている箇所を`ushow`に変えれば、お望みどおりの挙動になります。

```haskell
ghci> import Text.Show.Unicode
ghci> iimashita x = "今、" ++ ushow x ++ "って言いました！？"
ghci> putStrLn (iimashita "ハスケル")
今、"ハスケル"って言いました！？
```

わくわくもんですね！

`print`の例も、`uprint`に変えれば🆗です。

```haskell
ghci> uprint "私、堪忍袋の緒が切れました！"
"私、堪忍袋の緒が切れました！"
```

ウルトラハッピーですね！！

さらに、次のコマンドをGHCiに入力すれば、GHCiに直接入力した日本語文字列もそのまま表示されるようになります。

```haskell
ghci> :set -interactive-print=uprint
ghci> "みんなで幸せゲットだよ！"
"みんなで幸せゲットだよ！"
```

カンペキ✨

えっ、常に`uprint`したいからいちいち`:set -interactive-print=uprint`するのが面倒くさい？  
そんなあなたは👇を`~/.ghci`に書くことけって～いでしょう。

```haskell
import qualified Text.Show.Unicode
:set -interactive-print=Text.Show.Unicode.uprint
```

# unicode-showの（ものすごく近い）将来

そんなunicode-showですが、残念ながら一昨年、作者である村主崇行さんが亡くなってしまいました[^nushio]。  
日本に住むHaskellerをサポートする日本Haskellユーザーグループとしては、このパッケージをメンテナンスし続けることに大きな意義があると判断し、私はこのパッケージを[Haskell-jp](https://github.com/haskell-jp/)のGitHubリポジトリーでメンテナンスすることにしました。  
以下がそのリポジトリーです。

[^nushio]: 村主崇行さんは「[すごいHaskellたのしく学ぼう！](https://shop.ohmsha.co.jp/shopdetail/000000001926/)」の翻訳を担当されるなど、unicode-show以外にも日本のHaskell界に多大な功績をもたらした方でした。

<https://github.com/haskell-jp/unicode-show>

といっても、メンテナーの名前や`LICENSE`ファイルを書き換えて最新版をアップロードして以降特に何もしていなかったのですが<small>（[バグはあるけど直すのも難しそう](https://github.com/nushio3/unicode-show/issues/2)だし、概ね使えるし）</small>、なんと先日、Pull requestが来ました！

[Do not show values eagerly by Kaiepi · Pull Request #4 · haskell-jp/unicode-show](https://github.com/haskell-jp/unicode-show/pull/4)

この修正を適用する前のunicode-showは、文字列全体を評価してからエスケープシーケンスを元に戻す、という挙動だったため、長い文字列を与えた場合や無限の長さの文字列を与えた場合に、なかなか<small>（あるいは永遠に）</small>結果が返ってこないという問題がありました。

```haskell
ghci> uprint (repeat "ああああ！")
-- 何も表示されず、Ctrl + C を押すまで処理が返らない
```

修正後はちゃんと遅延評価を利用することで、無限の長さの文字列でも少しずつ変換することができます。

```haskell
ghci> uprint (repeat "ああああ！")
["ああああ！","ああああ！", ... "ああああ！","ああInterrupted.
-- Ctrl + Cを押すまで出力し続ける
```

今日記事にした一番の理由はこの話をするためです。  
[Kaiepi](https://github.com/Kaiepi)さんありがとうございます！  
先ほどリリースしました！🎉

<http://hackage.haskell.org/package/unicode-show-0.1.0.4>

# （番外編）pretty-simpleも使おう

時間がないので詳しくは省略しますが、実は[pretty-simple](http://hackage.haskell.org/package/pretty-simple)というパッケージを使えば、日本語をそのまま出力するのに加えて、プリティープリントできます。

```haskell
ghci> import Text.Pretty.Simple
ghci> pPrint ["きーらーめーくー♪", "ほーしーの力でー♪", "あこがーれのー♪", "わーたーし描くよー♪"]
[ "きーらーめーくー♪"
, "ほーしーの力でー♪"
, "あこがーれのー♪"
, "わーたーし描くよー♪"
]
```

例ではわかりづらいですが、ちゃんと色も着けてくれます！  
それでは2020年も、Happy Haskell Hacking🎁
