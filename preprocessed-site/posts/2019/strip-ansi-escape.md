---
title: strip-ansi-escapeというパッケージをリリースしました
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: July 8, 2019
tags:
...
---

現職でHaskellを仕事で書き始めるようになってからというもの、度々小さなパッケージをリリースするようになりました。  
敢えてパッケージにするほどのものでもなさそうなぐらい小さなものが多いですが、もし再利用したくなったらな、という気持ちで書いております。

# なに作ったか

今回もメインの処理は100行にも満たないような小さなもので、また用途もニッチです。  
具体的には、[ANSIエスケープコード](https://en.wikipedia.org/wiki/ANSI_escape_code)を文字列から取り除く、ただそれだけです。  
使い方も極めてシンプル:

```haskell
ghci> import Data.String.AnsiEscapeCodes.Strip.Text

-- 現状Text型向けにしか作っていないため、OverloadedStringsを有効にした方が使いやすい
ghci> :set -XOverloadedStrings
ghci> import Data.Text

-- 出力すると下線付きで "hello" と表示されるANSIエスケープコード付きの文字列
ghci> "\x001B[4mhello\x001B[0m"
"\ESC[4mhello\ESC[0m"

ghci> stripAnsiEscapeCodes "\x001B[4mhello\x001B[0m"
"hello"
```

# なぜ作ったか

通常我々がANSIエスケープコードを扱うときは、**ユーザーのために**端末に文字列を分かりやすく表示したいときで、それをプログラムで再利用することは想定していません。  
そのためANSIエスケープコードを出力できるアプリケーションは、大抵の場合出力しないよう設定できる<small>（あるいは、出力先がttyでないことを検出して出力しない）</small>ようになっています。  
なので、プログラムがANSIエスケープコードの混ざった文字列を扱わざるを得ない、という事態は、何かがおかしい事態だと考えるべきとも言えるでしょう。

一体どういう事態なのかというと、それは私がずっと開発中の、対話的Haskell入門コンテンツ --- [「失敗しながら学ぶHaskell入門」](https://github.com/haskell-jp/makeMistakesToLearnHaskell) --- で出遭った事態でした。  
「失敗しながら学ぶHaskell入門」（以下、英語名を略して「mmlh」と呼びます）では、ユーザーが書いたHaskellのソースコードを受け取って、GHCにコンパイルさせることで、型エラーなどのエラーメッセージを取得しています。  
当初からmmlhはそれを簡単にパースしてユーザーへのヒントを出すのに使ったり、ユーザーにそのまま表示したりするのに使うため、`-fdiagnostics-color=always`というオプションをGHCに渡していました。  
これは、エラーメッセージにANSIエスケープコードを混ぜて色を着けるようになったGHC 8.2から導入されたオプションで、「エラーメッセージに必ず<small>（ANSIエスケープコードを使って）</small>色を着ける」というものです。  
GHCが出すエラーメッセージを「簡単にパース」しつつ「ユーザーにそのまま表示」する、という2つの要件を満たすためには、このオプションを利用して、強制的にエラーメッセージに色を着ける必要がありました。

さらに最近、GHCが出したエラーメッセージをファイルに保存して、[GitHubで閲覧できるようにする](https://github.com/haskell-jp/makeMistakesToLearnHaskell/issues/101)<small>（正確には、閲覧して各行にコメントできるようにする）</small>、という機能も追加したため、ANSIエスケープコードを取り除かざるを得なくなってしまいました。  
というのも、`-fdiagnostics-color=always`を有効にしている限り、GHCは必ずANSIエスケープコードをエラーメッセージに混ぜるので、ファイルに保存してGitHub上で表示する際、下記のように余計な文字として混ざってしまい、エラーメッセージが読みづらくなってしまうのです。

```
�[;1m16.hs:19:18: �[;1m�[31merror:�[0m�[0m�[;1m�[0m�[0m�[;1m
    • No instance for (Num ([Char], String))
        arising from a use of ‘countWords’
    • In the expression: countWords (concat wordsList)
      In an equation for ‘countMap’:
          countMap = countWords (concat wordsList)
      In the expression:
        do paths <- getArgs
           wordsList <- for paths scrapeWords
           let countMap = countWords (concat wordsList)
           for_ (toList countMap) catCount�[0m�[0m
�[;1m�[34m   |�[0m�[0m
�[;1m�[34m19 |�[0m�[0m   let countMap = �[;1m�[31mcountWords (concat wordsList)�[0m�[0m
�[;1m�[34m   |�[0m�[0m�[;1m�[31m                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^�[0m�[0m
�[0m�[0m�[0m
```

`-fdiagnostics-color=always`を有効にしなければこんな問題は起こらないのですが、そうすると今度はユーザーにエラーメッセージを表示させる際、色が着かなくなってしまいます。  
せっかくGHC 8.2以降を使っているのに色つきのエラーメッセージが見られないのは残念ですよね。  
GHCを2回実行することで、ユーザーに表示する用のエラーメッセージとファイルに保存する用のエラーメッセージを分けることもできますが、それでは効率が悪いでしょうし。

そんなわけで、GHCが出力するエラーメッセージを**ユーザーに端末上で表示する用途と、ANSIエスケープコードを解釈しない箇所で表示する用途**、両方に使用したくなったため、今回敢えてANSIエスケープコードを取り除くライブラリーを作りました。  
もし他に同じような事態に出遭った方がいらっしゃいましたら、試してみてください🙏

# 最近のmmlh

ついでにここ数ヶ月弊社でやっている、mmlhを使った社内勉強会のお話も書こうかと思いましたが、やっぱり社内でのことなんで、[会社のブログ](https://eng-blog.iij.ad.jp/)に書くことにします。  
多分今週中には上げますので乞うご期待！
