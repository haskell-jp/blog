---
title: AsteriusでHaskellの関数をJSから呼べるようにしてみた（けど失敗）（拡大版）
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: May 4, 2019
tags: Asterius, WebAssembly
...
---

先日、[Emscripten & WebAssembly night !! #7](https://emsn.connpass.com/event/121028/)というイベントにて、[Asterius](https://tweag.github.io/asterius/)というHaskellをWebAssemblyにコンパイルするツールについて紹介いたしました。  
資料はこちら👇です。

[AsteriusでHaskellの関数をJSから呼べるようにしてみた（けど失敗）](https://the.igreque.info/slides/2019-04-19-asterius.html#(1))

本日は、スライドの英語で書いていた箇所を和訳しつつ、いろいろ捕捉してブログ記事の形で共有します。

# 🔍Asteriusとは何か

冒頭でも触れたとおり、[Asterius](https://tweag.github.io/asterius/)はHaskellのソースを[WebAssembly](https://developer.mozilla.org/ja/docs/WebAssembly)にコンパイルするコンパイラーです。  
GHCのHEAD<small>（開発中のバージョン）</small>を都度フォークして、現在活発に開発中です。  
Template Haskellと、GHC標準におけるIOを行う関数（の大半）を除いた、すべての機能が利用できるようになっています。  
現状のWebAssemblyを実用する上で必要不可欠であろう、FFIもサポートされています。  
つまり、JavaScriptからWebAssemblyにコンパイルされたHaskellの関数を読んだり、HaskellからJavaScriptの関数を呼ぶことができます！  
何かしらのIO処理を行う場合は、基本的にこのFFIを使ってJavaScriptの関数を呼ぶことになります。

加えて、`ahc-cabal`という名前のコマンドで、cabalパッケージを利用することもできます。  
こちらは`cabal`コマンドの単純なラッパーです。`ahc-cabal new-build`などと実行すれば、外部のパッケージに依存したアプリケーションも、まとめてWebAssemblyにコンパイルできます。  
本格的に開発する上では欠かせないツールでしょう。

# 👍Asteriusのいいところ

Asteriusは、"A linker which performs aggressive dead-code elimination, producing as small WebAssembly binary as possible."と謳っているとおり、GHCのランタイムを抱えているにしては、比較的小さいWASMファイルを生成するそうです。  
というわけで手元で試してみたところ、下記のような結果になりました。

- 空っぽのプログラム（`main = return ()`しかしないソース）:
    - 36KB（`.wasm`ファイルのみ）。なかなかいい感じですね。
    - 168KB（実行時に必要な`.mjs`ファイルを含めた合計）。未圧縮でこれなら確かに十分軽いでしょう。Webpackなどで結合・minifyするともっと軽くできますし。
- 今回私が移植を試みたアプリ（詳細は後ほど）:
    - 1.9MB（`.wasm`ファイルのみ）。うーん、ちょっと苦しいような...😥。
    - 2.1MB（実行時に必要な`.mjs`ファイルを含めた合計）。`.mjs`ファイルの内容は特に変わりませんでした。

ちなみに、移植前の元のソースを含むアプリを、Linux 64bit向けのELFファイルとしてビルドして比較してみたところ、`.wasm`ファイルよりも少し小さいぐらいでした。  
詳細な内訳が気にはなりますが、今のソースですと大体これぐらいが限界なのかも知れません<small>（でもWASMは現状32bitバイナリー相当のはずだし、もう少し小さくならないものか...）</small>。

加えて、Asteriusを利用して開発すると、ほぼ最新のGHCの開発版が使える、というところも、新しもの好きなHaskellerをわくわくさせるところですね！<small>（今回はあいにく新しい機能について調べる余裕もなかったので、特に恩恵は受けてませんが...😅）</small>  
Asteriusは、GHCをフォークしていくつかの機能を追加して作られているものです。  
しかし幸いオリジナルとの差分が十分に小さく、作者が定期的にrebaseすることができています。  
詳細な違いは[About the custom GHC fork](https://tweag.github.io/asterius/custom-ghc/)にまとまっています。近い将来GHC本体に取り込まれそうな修正ばかりではないかと。

それからこれは、ブラウザーでHaskellを動かすことができるという点でAsteriusの競合に当たる、GHCJSと比較した場合の話ですが、FFIを利用して、JavaScriptから**直接**Haskellを呼ぶことができるようになっているのも、優れた点と言えるでしょう。  
GHCJSは[こちらのドキュメント曰く](https://github.com/ghcjs/ghcjs/blob/3959a9321a2d3e2ad4b8d4c9cc436fcfece99237/doc/foreign-function-interface.md#calling-haskell-from-javascript)、JavaScriptからHaskellを呼ぶ機能は備えてはいるものの、簡単ではないため、ドキュメントも書かれておらず、推奨されてません。  
これでは状況によってはかなり使いづらいでしょう。  
今回私が試したように、コアとなる処理だけをHaskellの関数として書いて、それをJavaScriptから呼び出すということができないのです。

一方Asteriusでは、例えば👇のように書くことで、WASMがエクスポートする関数として、`func`をJavaScriptから呼べるようにすることができます！

```hs
foreign export javascript "func" func :: Int -> Int -> Int
```

ただし、実際に今回試してみたところ、Asteriusではまだバグがあったりしたので、この用途では依然使いにくいという状況ではありますが...（詳細は後で触れます）。

# 👎Asteriusのイマイチなところ

Asteriusは、やっぱりまだまだ開発中で、バグが多いです。  
今回の目的もバグのために果たせませんでした😢。
先ほども触れたとおり、特に未完成なのが、`IO`とTemplate Haskellです。  
GHCなら使えるはずの`IO`な関数の多くが使えませんし、Template Haskellに至っては一切利用できません。

`IO`については、現状、<small>（`putStrLn`などのよく使われる）</small>一部を除き、FFI<small>（`foreign import javascript`）</small>を使ってJavaScriptの関数経由でよばなけれなりません。  
これは、入出力関連のAPIを一切持たないという現状のWebAssemblyの事情を考えれば、致し方ない仕様だとも言えます。  
[WASI](https://github.com/WebAssembly/WASI)の策定によってこの辺の事情が変わるまでの間に、すべて`foreign import javascript`で賄うというのも、なかなか面倒なことでしょうし。

Template Haskellに関しては、現在[こちらのブランチ](https://github.com/tweag/asterius/pull/81)で開発中です。...と、思ったらこのPull request、Closeされてますね...。  
これに関して詳しい事情はわかりません。いずれにしても、Template Haskellを実装するには、コンパイル時にその場でHaskellを評価するためのインタープリターが別途必要だったりして、結構ハードルが高いのです。

加えて、RTS<small>（この場合、コンパイルしたHaskellを動かすのに必要なWASMやJavaScriptファイル）</small>が[`BigInt`](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/BigInt)に依存している関係で、V8やSpiderMonkeyでないと動かない点もまだまだ、という感じです。  
ブラウザーで言うと、2019年5月3日時点でChromeか、FirefoxのBeta版以降でないと使用できません[^firefox-stable]。

[^firefox-stable]: [Can I use](https://caniuse.com/#feat=bigint)曰く安定版でも`about:config`を書き換えればすでに使えるとのことなんですが、なぜか手元のFirefox 安定版ではうまくいきませんでした。確かに`about:config`にそれらしき設定はあるものの、`true`にしても何も変わらず...😰。  
ついでに細かいことを言うと、Firefox Nightlyは`about:config`を書き換えなくても使え、Beta版では`about:config`を書き換えると使えました。

# ⚙️Asteriusの仕組み

Asteriusのドキュメント「[IR types and transformation passes](https://tweag.github.io/asterius/ir/)」をざっくり要約してみると、Asteriusは以下のような流れで動くそうです。  
実際には`ahc-link`というコマンドがこれらの手順をまとめて実行するので、ユーザーの皆さんはあまり意識する必要はないでしょう。

1. [フロントエンドプラグイン](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#frontend-plugins)という仕組みでラップしたGHC<small>（のフォーク）</small>を使い、GHCが生成した、Cmmという中間言語で書かれたソースを、`AsteriusModule`という独自のオブジェクトに変換します。
1. `ahc-ld`という専用のリンカーで、WASM向けにリンクします。
1. 最後に、`ahc-dist`というコマンドで、リンクしたモジュールを実行できる状態にします。
    - [binaryen](https://github.com/WebAssembly/binaryen)か、[wasm-toolkit](https://github.com/tweag/asterius/tree/master/wasm-toolkit)というHaskellでWASMを書く言語内DSLを利用して、`ahc-ld`がリンクしたモジュールを検証し、`.wasm`ファイルに変換して、
    - 実行時に必要なJavaScriptファイルをコピーして、
    - Haskellのソースにおける`main`関数を実行する、エントリーモジュールを作ります。  
      あとはこれをHTMLファイルから`<script>`タグで参照すれば、ブラウザー上でHaskellが動きます。

# AsteriusでHaskell製の関数を実行してみた

ここからは、私が以前作った[アプリケーション](https://github.com/igrep/igrep-cashbook/tree/master/hs2)のコアに当たる関数をAsteriusでコンパイルすることで、ブラウザー上で動かせるようチャレンジした時の体験談を紹介します。

今回試みたアプリケーションは、単純なコマンドラインアプリケーションです。  
詳細は省きますが、行単位で書かれたファイルをパースして、項目ごとの合計を計算するだけの、ありふれたものです。  
パーサーは[megaparsec](http://hackage.haskell.org/package/megaparsec)を使って作り、整数の四則演算ができるようなっているのも特徴です。  
そのアプリケーションの処理のほとんどすべてに当たる、ファイル名とその中身を受け取って、計算結果を文字列で返す関数（`FilePath -> Text -> Text`）を、FFIでエクスポート<small>（`foreign export javascript`）</small>し、JavaScriptから呼べるようにしてみました。

アプリケーション自体の書き換えはほとんど必要なかったものの、依存関係を減らしたり、依存するパッケージを書き換えたりするのが大変でした。  
というのも、先ほど触れたとおり、Asteriusは現状「Template Haskellと、GHC標準におけるIOを行う関数（の大半）」が一切使用できないので、取り除かなければコンパイルエラーになってしまいます。  
template-haskellパッケージに間接的に依存しているだけで依存関係の解決すらできないのはなかなかつらいものでした。  
[`stack dot`](https://docs.haskellstack.org/en/stable/dependency_visualization/)コマンドを使って依存関係のツリーを作り、それを見てtemplate-haskellパッケージに間接的に依存しているパッケージを割り出し、そのパッケージの必要な関数のみを切り出すことでどうにか回避できました。  
[monoidal-containers](http://hackage.haskell.org/package/monoidal-containers)パッケージと[foldl](http://hackage.haskell.org/package/foldl)パッケージがそれでした。  
幸い、どちらも依存しているのはごく一部だったで、必要な部分だけをコピペして使うことにしました。  
それから、`IO`への依存もなくすために、[text](http://hackage.haskell.org/package/text)パッケージから`*.IO`なモジュールを取り除いたりもしました。

当然、元々のアプリケーションもtextパッケージの`*.IO`なモジュールを使ってはいたので、それを使わないよう修正する必要がありました。  
しかしそこはHaskell。そうした`IO`に依存した関数から純粋な関数を切り出すのは、型システムのおかげで大変楽ちんでした！😤  
入出力をするのにJavaScriptのFFIを使わないといけない、という現状のWebAssemblyの制約が、偶然にもマッチしたわけですね！  
純粋じゃない関数はときめかないので捨て去ってしまいましょう✨

## 結果

ここまで頑張った結果、目的の関数を`foreign export javascript`してコンパイルを通すことはできました🎉  
しかし、実際にブラウザー上で動かしてみたところ、[AsteriusのFFIのバグ](https://github.com/tweag/asterius/issues/105)にハマってしまいました...😢
肝心の`foreign export javascript`した関数が、返すべき値を返してくれないのです！  
恐らく`foreign export javascript`を使わずに、Haskell側からJavaScriptの関数を呼ぶようにしていれば、今回の問題は回避できたのではないかと思います。  
しかし、それは今回のゴールではありませんし、あまり便利ではないのでひとまず移植は見送ることにしました。残念！

# ✅おわりに

今回Asteriusを試したことで、ブラウザー上でHaskellを動かす、もう一つの可能性を知ることができました。  
とは言え、バグが多かったり依存関係から`IO`やTemplate Haskellを抜き出さなければならなかったりで、まだまだ実用的とは言い難いでしょう。  
しかし、今回報告したバグが直れば、ブラウザーによる処理のコアに当たる部分をHaskellで書く、という応用が利きそうです。  
例えばPandocなどHaskell製アプリケーションを、ブラウザーから操作する、なんてアプリケーション作りが捗りそうですね！
