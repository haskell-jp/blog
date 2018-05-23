---
title: Errors and the workarounds frequently encountered when dealing with Haskell on Windows
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
subHeading: Quick-and-dirty checklist
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: May 15, 2018
tags:
...
---

This is the English version of [WindowsでHaskellを扱う時によく遭遇するエラーと対処法](https://haskell.jp/blog/posts/2017/windows-gotchas.html).  
The original article is the 4th article of [Haskell (その4) Advent Calendar 2017 (Japanese)](https://qiita.com/advent-calendar/2017/haskell4).

この記事は、[Haskell (その4) Advent Calendar 2017](https://qiita.com/advent-calendar/2017/haskell4)14日目の記事です。

What I'm going to tell is summarized as [just one tweet (originally in Japanese)](https://twitter.com/igrep/status/938056578934042626):

> What I've learned:
>
> - chcp65001 if 'Invalid character'
> - rebuild if 'Permission Denied'
> - Don't mix Japanese characters in file paths.
> - Some libraries in C are available, and others are not.
>
> Perhaps they're helpful in other languages.

Let me add more details.

すごく簡潔にまとめるとこの間の下記のツイートに収まるのですが、もう少し丁寧に補足するために書きます。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">学んだことをまとめると<br>- Invalid characterと言われたらchcp 65001しよう<br>- Permission Deniedと言われたらビルドし直そう<br>- 日本語のパスが混ざらないよう気をつけよう<br>- Cのライブラリーはものによる<br>ですか。多分 <a href="https://twitter.com/hashtag/haskell?src=hash&amp;ref_src=twsrc%5Etfw">#haskell</a> 以外でも有益な話。</p>&mdash; Yuji Yamamoto: 山本悠滋 (@igrep) <a href="https://twitter.com/igrep/status/938056578934042626?ref_src=twsrc%5Etfw">2017年12月5日</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

# chcp 65001 if "Invalid character"
# Invalid characterと言われたらchcp 65001しよう

You would have encountered this frequently, especially if you don't know how to avoid/fix this.  
恐らく一番高確率で遭遇する & 知らないと回避できないのがこれ。  
Oh, it's caused again by building with hakyll!

あ、ほらまたhakyllでビルドしたら起きた！

```
> stack exec -- site rebuild
...
  [ERROR] preprocessed-site\posts/2017/01-first.md: hGetContents: invalid argument (invalid byte sequence)
```

The object called [`Handle`](https://www.stackage.org/haddock/lts-10.0/base-4.10.1.0/System-IO.html#t:Handle), used by GHC to read and write a file, knows its character encoding.

GHCがファイルを読み書きする時に使う[`Handle`](https://www.stackage.org/haddock/lts-10.0/base-4.10.1.0/System-IO.html#t:Handle)というオブジェクトには、文字コードの情報が含まれています。

This resembles Ruby's [`IO`](https://ruby-doc.org/core-2.5.0/IO.html) and Perl's file handler.  
Both of them represent the "gateway" of data, and assigning character encoding to them enables us to handle the only, consistently encoded strings by converting the incoming data.  
これはRubyの[`IO`](https://docs.ruby-lang.org/ja/latest/class/IO.html)やPerlのファイルハンドラーにあるような仕組みと大体似ていて、`Handle`といったデータの「入り口」を表すオブジェクトに文字コードを紐付けることで、外から入ってくる文字列の文字コードを確実に内部の統一された文字コードに変換してくれます。  
In Haskell's type `Char`, the only default encoding is UTF-32 (is this the right name in this case?).

Haskellの`Char`型の場合はUTF-32（この場合その言い方でよかったっけ？）のはずです。

The character encoding assinged to a `Handle` by default depends on the locale settings of the OS: in Japanese Windows, Windows-31J (a.k.a CP932).  
この`Handle`に紐付ける文字コード、当然のごとくデフォルトではOSのロケール設定に従って設定されるようになってまして、日本語版のWindowsではそう、Windows-31J（またの名をCP932）ですね。  
But it's now soon becoming 2018 (when writing the original article). The most of the file you create should be in UTF-8 unless you write programs in notepad.exe[^notepad].  
でも今はもうすぐ2018年。あなたが「メモ帳」でプログラムを書く人でもない限り、新しく作るファイルの大半はUTF-8でしょう。  
It doesn't work to read a UTF-8 file as a Windows-31J file because they're very different encoding system.  
UTF-8とWindows-31Jは全然違う体系の文字コードなので、UTF-8なファイルをWindows-31Jのファイルとして読もうとしてもうまくいかないわけです。  
The `invalid byte sequence` error, shown at the head of this section, is caused by that inconsistency.  
冒頭にあげた`invalid byte sequence`というエラーはまさにそうした場合に起こるエラーです。  
Remember this kind of errors are often caused when reading or writing stdout/stdin, as well as plain files.

ファイルの読み書きだけでなく標準入出力でもしばしば発生するので覚えておいてください。

[^notepad]: Translator's note: In Japanese locale, notepad.exe saves the file in Windows-31J. This will be changed (into UTF-8) in the future release of Windows 10.

## Workaround
## 対策

### If you encounter as a user
### ユーザーとして出くわした場合

In many cases you can avoid these kind of errors by running the below command in advance.

多くの場合、このエラーは以下のコマンドをあらかじめ実行しておけば回避できます。

```
> chcp 65001
> stack exec -- site rebuild
... Should work!
```

This command temporarily changes the character encoding in the current Command Prompt session.  
これは、現在開いているコマンドプロンプトで一時的に文字コードを切り替えるコマンドです。  
The number `65001` seems to stand for UTF-8.  
`65001`という数字がUTF-8を指しているようです。  
To roll it back, run `chcp 932`.

もとに戻したい場合は`chcp 932`と実行しましょう。

```
> chcp 932
```

It seems that the "932" of "CP932" is the same "932" entered here!

どうやら「CP932」の「932」はここで出てくる「932」と同じものを指しているようですね！

The `chcp` command is available in MSYS2's bash (Suprises me a little. How it works?).  
どういう仕様なのか分かりませんが、このコマンド、MSYS2のbashでも使用できます。  
But you should know that `chcp` exists at `C:\Windows\System32\`, which MSYS2 users usually don't want include in the `PATH`.  
ただし`chcp`コマンドは`C:\Windows\System32\`という、MSYS2ユーザーにとってはあまり`PATH`に入れたくない場所に入っています。  
The directory contains many incompatible commands whose names conflict with the tools loved by Unix people (e.g. `find.exe`)!

このディレクトリーには、`find.exe`など、Unixな方が好んで使うコマンドと同じ名前の非互換なコマンドがゴロゴロ転がっているのです！

So I've dropped `C:\Windows\System32\` from `PATH when using MSYS2.  
なので私はMSYS2を使う時は`C:\Windows\System32\`は`PATH`から抜いています。  
If you've done like me, run by full path:

私と同じような方は下記のようにフルパスで実行しましょう。

```
/c/Windows/System32/chcp.com 932
```

### If still it doesn't work, or you're the developer of the libraries etc.
### それでもダメな場合、あるいはライブラリーなどの開発者として出くわした場合

Unfortunately, the error can often persist even after running `chcp 65001`[^eta-20127].  
残念ながら、`chcp 65001`してもこのエラーが消えないことはあります[^eta-20127]。  
According to my guess, the `chcp 65001` command doesn't affect the grandchild processes of the Command Prompt (or bash etc.) on which the `chcp` is run (i.e. the child processes of the command you enter).
私の推測なんですが、どうも`chcp 65001`は`chcp 65001`したコマンドプロンプト（とかbash）の孫プロセス（つまり、あなたが入力したコマンドの子プロセス）には届かないことがあるようです。

[^eta-20127]: By the way, when I once tried to build the compiler of [Eta](http://eta-lang.org/), (as far as I remember) `chcp 65001` didn't fix the problem, but `chcp 20127` did.  
As `chcp 20127` switches into US-ASCII, so I suspect the local environment of the developer of Eta is US-ASCII...

If the error still happens you can either report to the developer, or fix it yourself!  
そんなときは、実際にエラーが起きているコマンドの開発元にバグ報告するか、自分で直してみましょう。  
When reporting; asking the developer to run after doing `chcp 932' could help him/her reproduce the bug (Sorry, I've never tried it).  
バグ報告する場合は、「`chcp 932`してから実行してみて」とお願いすると、バグ報告を受けた開発者も再現しやすくて助かるかも知れません（残念ながら私はやったことがありません）。  
When fixing by yourself, perhaps the best and most certain way would be to switch the character encoding of the `Handle` object.

自分で直す場合、いろいろ方法はありますが、対象の`Handle`オブジェクトの文字コードを変えることで対処するのが、一番直接的で確実でしょう。

This problem is caused by the inconsistency between the `Handle`'s character encoding and the actually transferred bytes' encoding. So switching into the proper encoding should fix it.  
この問題は`Handle`に設定された文字コードと実際にやりとりされる文字列の文字コードに食い違いが発生しているため起こるものなのですから、適切な文字コードに変えてしまえばいいのです。  
If the error happenes when reading/writing a common UTF-8 file via the `Handle`, writing like below can avoid it:

状況にもよりますがエラーが起きた`Handle`が普通のUTF-8なファイルを読み書きするものである場合、下記のようにすれば、問題は回避できるはずです。

```haskell
import System.IO (hSetEncoding)
import GHC.IO.Encoding (utf8)

hSetEncoding handle utf8
```

As a bonus, I'll show you an example of how [I myself addressed a problem caused by the standard output (or standard error output), and fixed a bug in haddock](https://github.com/haskell/haddock/pull/566).
それから、[実際に私がhaddockのバグを直した時](https://github.com/haskell/haddock/pull/566)を例に標準出力（または標準エラー出力）でこのエラーが発生した時の対応も紹介しておきます。  
In short, it can at least suppress the error to paste the code below before your program uses the `Handle` (Copied from [this commit](https://github.com/haskell/haddock/pull/566/commits/855118ee45e323fd9b2ee32103c7ba3eb1fbe4f2)).

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

CPP macros to `import` modules only available on Windows makes this code hard to read, so let's cut out the verbose part:

Windowsでしか使用できないモジュールを`import`している関係上、CPPのマクロが混ざって読みにくいですが、重要な部分だけ切り出すと、

```
hSetEncoding stdout $ mkLocaleEncoding TransliterateCodingFailure
```

とすればよいのです。

Here're the details:  
一つ一つ解説しましょう。  
First of all, `hSetEncoding` is the function to change the `Handle`'s character encoding, as I referred before.  
まず`hSetEncoding`は先ほども触れたとおり指定した`Handle`の文字コードを変更する関数です。  
Then `stdout` is the `Handle` for the standard output as its name.  
そして`stdout`は名前の通り標準出力を表す`Handle`です。  
The last function call `mkLocaleEncoding TransliterateCodingFailure` returns a character encoding object for the current Windows' character encoding (i.e. `chcp`ed character encoding), configured as "Even if the `Handle` detects any characters which can't be converted into/from a Unicode character, don't raise an error, convert it into some likable character instead.".  
最後の`mkLocaleEncoding TransliterateCodingFailure`ですが、これはWindowsで設定された文字コード（`chcp`された文字コードと同じ）を作って、「もし（Unicodeから、あるいはUnicodeに）変換できない文字があった場合、エラーにせず、それっぽい文字に変換する」という設定で返す、という意味です。

As the result of the `hSetEncoding` above, and the current character encoding is Windows-31J, the character used in the compilation error of GHC:
結果、`chcp 932`な状態でGHCのエラーメッセージにも使われる

```
↓This character
• No instance for (Transformation Nagisa CardCommune_Mepple)
↑
```

```
↓この文字
• No instance for (Transformation Nagisa CardCommune_Mepple)
↑
```

is converted into

が、

```
? No instance for (Transformation Nagisa CardCommune_Mepple)
```

the question mark. Yeah, this is the "?" I bet most users of GHC on Japanese Windows have seen at least once 😅  
のように、クエスチョンマークに変換されるようになります。そう、日本語のWindowsでGHCをお使いの方は一度は目にした「?」ではないでしょうか😅  
This makes me guess GHC executes `hSetEncoding stderr $ mkLocaleEncoding TransliterateCodingFailure` by default before printing out the compilation error.  
つまりGHCはデフォルトで`hSetEncoding stderr $ mkLocaleEncoding TransliterateCodingFailure`しているものと推測されます。  
Anyway, it's good that the program doesn't abort due to the error!

いずれにせよ、エラーが起きないだけマシですね。

As the last note of this section: Read [the document of GHC.IO.Encoding](https://hackage.haskell.org/package/base-4.10.1.0/docs/GHC-IO-Encoding.html) for the details of how GHC handles various character encodings.  
更に補足すると、GHCの文字コードについてより詳しい情報は、[GHC.IO.Encodingのドキュメント](https://hackage.haskell.org/package/base-4.10.1.0/docs/GHC-IO-Encoding.html)をご覧ください。

# Rebuild if "Permission Denied"
# Permission Deniedと言われたらビルドし直そう

I've made the first section too long for "Quick-and-dirty checklist", but I'll tell you in short from this section.  
雑なまとめと言いつつ最初の一つ目が長くなってしまいましたが、ここからは簡単に言います。  
We often encounter some errors like "Permission Denied", "Directory not empty" and similar ones when running `stack build`, `ghc`, `elm-make`, and any other commands written in Haskell.  
Windowsで`stack build`なり`ghc`なり`elm-make`なりとにかくいろいろ動かしていると、「Permission Denied」と言ったエラー（あるいはこれと似たようなメッセージのエラー）に出遭います。  
To tell the truth, I'm completely not sure of the cause, but those errors disappear by running the same command several times.  
正直に言って私は原因はサッパリ分かってないのですが、このエラーは大抵の場合何度も同じコマンドを実行すれば再現しませんでした。  
The key is to repeat many times. Never give up only by once or twice 😅  
一度や二度ではめげず、繰り返すのがポイントです 😅  
Turning off your antivirus software's scanning of the problematic directory, Dropbox's synchronisatin, etc. might also fix such errors.
問題が起きているディレクトリーをウィルス対策ソフトのスキャン対象から外してみるとか、Dropboxの同期を一時的に止めてみる、といったこともやってみるといいかもしれません。

あ、あと、「Directory not empty」みたいなのもあったかな。これは同類のはずです。

# Try hard to build libraries in C...
# Cのライブラリーは... まぁ、頑張れ。

On Windows, it frequetly troubles us to install libraries which depend on libraries written in C (registered as `lib***` in your OS's package manager).  
Pure Haskellなライブラリーであれば大体OKなんですが、残念ながらCのライブラリー（`lib***`みたいな名前でよくOSのパッケージマネージャーに登録されているやつですね）に依存したライブラリーは、Windowsでインストールするのは結構トラブることが多いです。  
But this is not the case only for Haskell.

まぁ、これはHaskellに限った話ではないでしょう。

The way to fix depends on the case, so let me give you some examples as external links (Sorry, all pages are written in Japanese!).

対応方法は私が知る限り完全にケースバイケースなので、ここでは知っている対応例をいくつか挙げておきましょう。

- HDBC-sqlite3:
    - [Windows版stackでもHDBC-sqlite3をビルドする - Qiita](https://qiita.com/igrep/items/d947ab871eb5b20b57e4)
    - [MSYS2でHDBC-sqlite3をコンパイル - 北海道苫小牧市出身の初老PGが書くブログ](http://hiratara.hatenadiary.jp/entry/2017/01/29/110100)
- [Haskell - Haskellにてstackでiconvパッケージを利用する方法【Windows環境】(102462)｜teratail](https://teratail.com/questions/102462)

That's all!  
以上です！  
Then, Happy Hacking in Haskell on Windows 10!! I don't know WSL!🏁🏁🏁
それでは2018年もHaskell on Windows 10でHappy Hacking!! WSLなんて知らないぜ！🏁🏁🏁
