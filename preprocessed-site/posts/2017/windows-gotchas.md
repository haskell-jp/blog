---
title: WindowsでHaskellを扱う時によく遭遇するエラーと対処法
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
subHeading: 雑なまとめ
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: December 25, 2017
tags:
...
---

この記事は、[Haskell (その4) Advent Calendar 2017](https://qiita.com/advent-calendar/2017/haskell4)14日目の記事です。  
枠が空いていたので埋めるために登録しました。  
長くかかった割には実験自体は失敗気味な、[昨日のこちらの記事](https://haskell.jp/blog/posts/2017/typesafe-precure2.html)よりは有用な情報じゃないかと思います。  
ほかの言語でもありそうな話ですしね。

すごく簡潔にまとめるとこの間の下記のツイートに収まるのですが、もう少し丁寧に補足するために書きます。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">学んだことをまとめると<br>- Invalid characterと言われたらchcp 65001しよう<br>- Permission Deniedと言われたらビルドし直そう<br>- 日本語のパスが混ざらないよう気をつけよう<br>- Cのライブラリーはものによる<br>ですか。多分 <a href="https://twitter.com/hashtag/haskell?src=hash&amp;ref_src=twsrc%5Etfw">#haskell</a> 以外でも有益な話。</p>&mdash; Yuji Yamamoto: 山本悠滋 (@igrep) <a href="https://twitter.com/igrep/status/938056578934042626?ref_src=twsrc%5Etfw">2017年12月5日</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

# Invalid characterと言われたらchcp 65001しよう

恐らく一番高確率で遭遇する & 知らないと回避できないのがこれ。  
あ、ほらまたhakyllでビルドしたら起きた！

```
> stack exec -- site rebuild
...
  [ERROR] preprocessed-site\posts/2017/01-first.md: hGetContents: invalid argument (invalid byte sequence)
```

GHCがファイルを読み書きする時に使う[`Handle`](https://www.stackage.org/haddock/lts-10.0/base-4.10.1.0/System-IO.html#t:Handle)というオブジェクトには、文字コードの情報が含まれています。

これはRubyの[`IO`](https://docs.ruby-lang.org/ja/latest/class/IO.html)やPerlのファイルハンドラーにあるような仕組みと大体似ていて、`Handle`といったデータの「入り口」を表すオブジェクトに文字コードを紐付けることで、外から入ってくる文字列の文字コードを確実に内部の統一された文字コードに変換してくれます。  
Haskellの`Char`型の場合はUTF-32（この場合その言い方でよかったっけ？）のはずです。

この`Handle`に紐付ける文字コード、当然のごとくデフォルトではOSのロケール設定に従って設定されるようになってまして、日本語版のWindowsではそう、Windows-31J（またの名をCP932）ですね。  
でも今はもうすぐ2018年。あなたが「メモ帳」でプログラムを書く人でもない限り、新しく作るファイルの大半はUTF-8でしょう。  
UTF-8とWindows-31Jは全然違う体系の文字コードなので、UTF-8なファイルをWindows-31Jのファイルとして読もうとしてもうまくいかないわけです。  
冒頭にあげた`invalid byte sequence`というエラーはまさにそうした場合に起こるエラーです。  
ファイルの読み書きだけでなく標準入出力でもしばしば発生するので覚えておいてください。

## 対策

### ユーザーとして出くわした場合

多くの場合、このエラーは以下のコマンドをあらかじめ実行しておけば回避できます。

```
> chcp 65001
> stack exec -- site rebuild
... 動くはず！
```

これは、現在開いているコマンドプロンプトで一時的に文字コードを切り替えるコマンドです。  
`65001`という数字がUTF-8を指しているようです。  
もとに戻したい場合は`chcp 932`と実行しましょう。

```
> chcp 932
```

どうやら「CP932」の「932」はここで出てくる「932」と同じものを指しているようですね！

どういう仕様なのか分かりませんが、このコマンド、MSYS2のbashでも使用できます。  
ただし`chcp`コマンドは`C:\Windows\System32\`という、MSYS2ユーザーにとってはあまり`PATH`に入れたくない場所に入っています。  
このディレクトリーには、`find.exe`など、Unixな方が好んで使うコマンドと同じ名前の非互換なコマンドがゴロゴロ転がっているのです！

なので私はMSYS2を使う時は`C:\Windows\System32\`は`PATH`から抜いています。  
私と同じような方は下記のようにフルパスで実行しましょう。

```
/c/Windows/System32/chcp.com 932
```

### それでもダメな場合、あるいはライブラリーなどの開発者として出くわした場合

残念ながら、`chcp 65001`してもこのエラーが消えないことはあります[^eta-20127]。  
私の推測なんですが、どうも`chcp 65001`は`chcp 65001`したコマンドプロンプト（とかbash）の孫プロセス（つまり、あなたが入力したコマンドの子プロセス）には届かないことがあるようです。

[^eta-20127]: 敢えて脚注に書きますが、[Eta](http://eta-lang.org/)のコンパイラーをビルドしている時（のはず）、`chcp 65001`でもダメで`chcp 20127`ならうまくいったことがあります。  
`chcp 20127`はUS-ASCIIに切り替えるためのコマンドですが、やっぱりEtaの開発者の手元（？）ではそうなっているからなのでしょうか...？

そんなときは、実際にエラーが起きているコマンドの開発元にバグ報告するか、自分で直してみましょう。  
バグ報告する場合は、「`chcp 932`してから実行してみて」とお願いすると、バグ報告を受けた開発者も再現しやすくて助かるかも知れません（残念ながら私はやったことがありません）。  
自分で直す場合、いろいろ方法はありますが、対象の`Handle`オブジェクトの文字コードを変えることで対処するのが、一番直接的で確実でしょう。

この問題は`Handle`に設定された文字コードと実際にやりとりされる文字列の文字コードに食い違いが発生しているため起こるものなのですから、適切な文字コードに変えてしまえばいいのです。  
状況にもよりますがエラーが起きた`Handle`が普通のUTF-8なファイルを読み書きするものである場合、下記のようにすれば、問題は回避できるはずです。

```haskell
import System.IO (hSetEncoding)
import GHC.IO.Encoding (utf8)

hSetEncoding handle utf8
```

それから、[実際に私がhaddockのバグを直した時](https://github.com/haskell/haddock/pull/566)を例に標準出力（または標準エラー出力）でこのエラーが発生した時の対応も紹介しておきます。  
コードだけ貼り付けると、下記のようにすれば少なくともエラーが起こらないようにすることはできます（[このコミット](https://github.com/haskell/haddock/pull/566/commits/855118ee45e323fd9b2ee32103c7ba3eb1fbe4f2)とほぼ同じ内容です）。

```haskell
{-# LANGUAGE CPP #-}

import System.IO (hSetEncoding, stdout)

#if defined(mingw32_HOST_OS)
import GHC.IO.Encoding.CodePage (mkLocaleEncoding)
import GHC.IO.Encoding.Failure (CodingFailureMode(TransliterateCodingFailure))
#endif

...

#if defined(mingw32_HOST_OS)
  liftIO $ hSetEncoding stdout $ mkLocaleEncoding TransliterateCodingFailure
#endif
```

Windowsでしか使用できないモジュールを`import`している関係上、CPPのマクロが混ざって読みにくいですが、重要な部分だけ切り出すと、

```
hSetEncoding stdout $ mkLocaleEncoding TransliterateCodingFailure
```

とすればよいのです。

一つ一つ解説しましょう。  
まず`hSetEncoding`は先ほども触れたとおり指定した`Handle`の文字コードを変更する関数です。  
そして`stdout`は名前の通り標準出力を表す`Handle`です。  
最後の`mkLocaleEncoding TransliterateCodingFailure`ですが、これはWindowsで設定された文字コード（`chcp`された文字コードと同じ）を作って、「もし（Unicodeから、あるいはUnicodeに）変換できない文字があった場合、エラーにせず、それっぽい文字に変換する」という設定で返す、という意味です。

結果、`chcp 932`な状態でGHCのエラーメッセージにも使われる

```
↓この文字
• No instance for (Transformation Nagisa CardCommune_Mepple)
↑
```

が、

```
? No instance for (Transformation Nagisa CardCommune_Mepple)
```

のように、クエスチョンマークに変換されるようになります。そう、日本語のWindowsでGHCをお使いの方は一度は目にした「?」ではないでしょうか😅  
つまりGHCはデフォルトで`hSetEncoding stderr $ mkLocaleEncoding TransliterateCodingFailure`しているものと推測されます。  
いずれにせよ、エラーでプログラムが異常終了しないだけマシですね。

更に補足すると、GHCの文字コードについてより詳しい情報は、[GHC.IO.Encodingのドキュメント](https://hackage.haskell.org/package/base-4.10.1.0/docs/GHC-IO-Encoding.html)をご覧ください。

# Permission Deniedと言われたらビルドし直そう

雑なまとめと言いつつ最初の一つ目が長くなってしまいましたが、ここからは簡単に言います。  
Windowsで`stack build`なり`ghc`なり`elm-make`なりとにかくいろいろ動かしていると、「Permission Denied」と言ったエラー（あるいはこれと似たようなメッセージのエラー）に出遭います。  
正直に言って私は原因はサッパリ分かってないのですが、このエラーは大抵の場合何度も同じコマンドを実行すれば再現しませんでした。  
一度や二度ではめげず、繰り返すのがポイントです 😅  
問題が起きているディレクトリーをウィルス対策ソフトのスキャン対象から外してみるとか、Dropboxの同期を一時的に止めてみる、といったこともやってみるといいかもしれません。

あ、あと、「Directory not empty」みたいなのもあったかな。これは同類のはずです。

# Cのライブラリーは... まぁ、頑張れ。

Pure Haskellなライブラリーであれば大体OKなんですが、残念ながらCのライブラリー（`lib***`みたいな名前でよくOSのパッケージマネージャーに登録されているやつですね）に依存したライブラリーは、Windowsでインストールするのは結構トラブることが多いです。  
まぁ、これはHaskellに限った話ではないでしょう。

対応方法は私が知る限り完全にケースバイケースなので、ここでは知っている対応例をいくつか挙げておきましょう。

- HDBC-sqlite3:
    - [Windows版stackでもHDBC-sqlite3をビルドする - Qiita](https://qiita.com/igrep/items/d947ab871eb5b20b57e4)
    - [MSYS2でHDBC-sqlite3をコンパイル - 北海道苫小牧市出身の初老PGが書くブログ](http://hiratara.hatenadiary.jp/entry/2017/01/29/110100)
- [Haskell - Haskellにてstackでiconvパッケージを利用する方法【Windows環境】(102462)｜teratail](https://teratail.com/questions/102462)

以上です！  
それでは2018年もHaskell on Windows 10でHappy Hacking!! WSLなんて知らないぜ！🏁🏁🏁
