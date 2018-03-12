---
title: WindowsでHaskellを扱う時によく遭遇するNo such file or directoryについて
headingBackgroundImage: ../../img/post-bg.jpg
headingDivClass: post-heading
subHeading: 短いパスにしよう
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: March 13, 2018
tags:
...
---

去年、[WindowsでHaskellを扱う時によく遭遇するエラーと対処法](/posts/2017/windows-gotchas.html)という記事で、WindowsユーザーがHaskellで開発したとき、あるいはHaskell製のプログラムを使用した際によく遭遇するエラーやその回避方法を紹介しました。  
今回は、そこに追記したい内容として、最近私がよく出遭うようになったエラーを紹介します。

# `openFile: does not exist (No such file or directory)`といわれたら短いパスに移そう

`does not exist (No such file or directory)`というエラーは、本当に読んで字のごとく、開こうとしたファイルが存在しないためのエラーであることとがもちろん多いのですが、エラーメッセージに反して違う原因である場合もあります。

例えば、最近私はとあるプロジェクトを数文字長い名前にリネームしたのですが、たったそれだけで、`stack test`した際必ず問題のエラーが発生するようになってしまいました。

```
$ stack test
a-little-longer-name-project-0.1.0.0: build (lib + exe + test)
Preprocessing library for a-little-longer-name-project-0.1.0.0..
Building library for a-little-longer-name-project-0.1.0.0..
Preprocessing executable 'mmlh' for a-little-longer-name-project-0.1.0.0..
Building executable 'mmlh' for a-little-longer-name-project-0.1.0.0..
Preprocessing test suite 'a-little-longer-name-project-test' for a-little-longer-name-project-0.1.0.0..
Building test suite 'a-little-longer-name-project-test' for a-little-longer-name-project-0.1.0.0..
[1 of 5] Compiling Paths_aLittleLongerNameProject ( .stack-work\dist\5c8418a7\build\a-little-longer-name-project-test\autogen\Paths_aLittleLongerNameProject.hs, .stack-work\dist\5c8418a7\build\a-little-longer-name-project-test\a-little-longer-name-project-test-tmp\Paths_aLittleLongerNameProject.o )
.stack-work\dist\5c8418a7\build\a-little-longer-name-project-test\a-little-longer-name-project-test-tmp\.stack-work\dist\5c8418a7\build\a-little-longer-name-project-test\autogen\Paths_aLittleLongerNameProject.dump-hi: openFile: does not exist (No such file or directory)
```

どういうことかと悩んでいたところ、[こんなIssue](https://github.com/commercialhaskell/stack/issues/3649)を見つけました。  
[Snoymanの指摘](https://github.com/commercialhaskell/stack/issues/3649#issuecomment-351612621)のとおり、こちらの問題はWindowsで使えるパスの長さが原因のエラーのようです。  
どういうことかというと、[MSDNのこちらのページ](https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247%20\(v=vs.85\)#maxpath)でも触れているとおり、Windowsの（C言語レベルでの）各種ファイル操作用APIでは、一度に扱えるパスの長さが260文字までと決められていて、その制限にかかったためのエラーだというのです！  
`does not exist (No such file or directory)`なんてエラーメッセージで表されるのでわかりづらい！<small>（おそらくWindowsのエラーコードの出し方に問題があるんじゃないかと思います）</small>

DOS時代から残るこの制限、完全に時代錯誤なものでしかないのですが、Windowsでパッケージマネージャーなどが自動的に作ったパスを扱っていると、しばしば出くわすことがあります。  
stackにおいても、[こちらのIssue](https://github.com/commercialhaskell/stack/issues/3285)で同じ問題が議論されていたり、[ver. 1.6.5のChangeLog](https://github.com/commercialhaskell/stack/releases/tag/v1.6.5)でも言及されていたりと、至る所で格闘している跡があります。

## 回避方法

そんな`does not exist (No such file or directory)`ですが、残念ながら私が知る限り、プロジェクトなどのパスを（`C:\`などのよりルートに近い場所に置いて）より短くする以外の回避方法はありません。  
[haskell-ide-engineのインストール方法のページ](https://github.com/haskell/haskell-ide-engine#installation-on-windows)曰く、（新しめの）Windows 10であれば、グループポリシーを編集して、「Win32の長いパスを有効にする」を「有効」にすれば回避できるとのことですが、残念ながら手元で試した限りうまくいきませんでした。何かやり方がまずかったのかもしれませんが。  
いずれにしても、`stack build`コマンドなどを実行したときに問題のエラーに遭遇した場合、ビルドしたいもののパスをなんとかして短くする以上の方法はありません。  
`C:\`直下をホームディレクトリのように使う人が今でもたくさんいるわけです。

一方、あなたが問題のエラーが発生するプログラムを**修正する**ことができる立場にある場合、次の方法で回避できるかもしれません（申し訳なくもいずれも手元では試しておりません。あしからず）。

### 長いパスをより短くするために、カレントディレクトリーを変更して、相対パスを短くする。

本件はあくまでも、Windowsの各種ファイル操作用APIの1回の呼び出しで渡せる長さの制限ですので、制限を超えてしまうような場合はパスを分割すればよいのです。  
[filepathパッケージの`splitFileName`関数](https://hackage.haskell.org/package/filepath-1.4.2/docs/System-FilePath-Posix.html#v:splitFileName)や[`splitPath`関数](https://hackage.haskell.org/package/filepath-1.4.2/docs/System-FilePath-Posix.html#v:splitPath)を駆使してパスを分割した上で、対象のファイルの親ディレクトリーまで[directoryパッケージの`setCurrentDirectory`関数](https://hackage.haskell.org/package/directory-1.3.2.1/docs/System-Directory.html#v:setCurrentDirectory)で移動すれば、制限に引っかからないはずです。

残念ながらカレントディレクトリーはプロセス全体で共有される情報ですので、マルチスレッドなプログラムでは頭の痛い問題が出てきてしまいますが、一番確実に回避できる方法のはずです。  
マルチスレッドである場合を考慮したくない場合は、次に紹介する方法を検討するとよいでしょう。

### Win32 APIのユニコード版の関数に、`\\?\`というプレフィックスを着けた絶対パスを渡す。

ここまでに出てきた、「Windowsの各種ファイル操作用API」は、すべて「Win32 API」と呼ばれるWindows固有のAPI群の一部です。  
この「Win32 API」に含まれる関数の多くは、「ユニコード版」とそうでないものに分かれます<small>（詳細は[Conventions for Function Prototypes (Windows)](https://msdn.microsoft.com/ja-jp/library/windows/desktop/dd317766\(v=vs.85\).aspx)をご覧ください）</small>。

このうち、「ユニコード版」のAPIには、この制限を緩和する専用の機能が含まれています。  
先ほども触れた[MSDNのページ](https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247%20\(v=vs.85\)#maxpath)曰く、なんと`\\?\`という変な文字列を絶対パスの頭に着けると、最大約32,767文字のパスまで受け付けるようになるというのです！  
なんともアドホックな感じのする解決方法ですが、Microsoftが言うんだから間違いありません。  
いずれにしても32,767文字という微妙な最大文字数ができてしまいますが、UTF-16での32,767文字なので、そう簡単に超えることはないでしょう。  
いちいち絶対パスに変えて変なプレフィックスを加えないといけないという面倒くささはありますが、いちいち分割して相対パスに変換するよりは簡単なはずですし、検討する価値があるでしょう。

# おわりに

さて、またしてもWindows固有の面倒な問題を紹介することとなってしまいましたが、俗世の喜び（主にゲーム）と簡単にインストールできるGUIに慣らされてしまった私は、今後もWindowsを使い続けるでしょう。  
いろいろ困難は尽きませんがこれからもWindowsでHappy Haskell Lifeを！🏁🏁🏁

# 参考URL

※本文中で言及していないもののみ

- [プログラマ的にWindows 10 Anniversary Updateのうれしいところ - kkamegawa's weblog](http://kkamegawa.hatenablog.jp/entry/2016/07/27/220014)
- [Windows 10 "Enable NTFS long paths policy" option missing - Super User](https://superuser.com/questions/1119883/windows-10-enable-ntfs-long-paths-policy-option-missing)
