---
title: HIW 2019で発表された、GHC 8.8で導入された機能
subHeading: ～HIW 2019参加レポート その1～
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: September 14, 2019
tags: GHC, Haskell Implementors' Workshop
...
---

こんにちは。  
今回からいくつか、「[Haskell Implementors' Workshop 2019](https://icfp19.sigplan.org/home/hiw-2019#About)」に私が先月参加した際のレポートとして、印象深い発表をテーマごとに分けた短い記事を執筆します。  
最近公開された[GHC 8.8](https://www.haskell.org/ghc/download_ghc_8_8_1.html)の話はもちろん、未来のGHCやその他のHaskellの処理系を知るのによいイベントでしたので、その一部だけでも伝われば幸いです。

# そもそもHaskell Implementors' Workshop (HIW)とは？

シリーズ（？）第1回目なので、簡単にHIWそのものについて紹介しておきましょう。  
HIWは、[ICFP (International Conference on Functional Programming)](https://icfp19.sigplan.org/home)という関数型プログラミングについての国際会議に併設された、Haskellの実装者のためのワークショップです。  
名前の通り、GHCをはじめとするHaskellの処理系<small>（あるいは、Haskellで実装された言語処理系）</small>の実装に関する発表だけでなく、かなり緩いテーマのLightning Talkの時間があったり、GHCの将来の方向性について自由に議論する時間もあったりしました。

今回はそのうち、掲題のとおり「HIW 2019で発表された、GHC 8.8で導入された機能」を紹介します。まずは「HIE files in GHC 8.8」から。

# HIE files in GHC 8.8

発表者: Zubin Duggal, Matthew Pickering *University of Bristol*

GHC 8.8で新たに追加された、HIE<small>（「Haskell Interface Extended」の略と思われます）</small>ファイルについての発表です。  
コンパイル時にGHCが得たモジュールの情報を、[Haskell IDE Engine](https://github.com/haskell/haskell-ide-engine)などのIDEのバックエンドが再利用しやすい形で出力する機能です。  
従来Haskell IDE Engine<small>（その裏で使われているghc-mod）</small>や[ghcid](https://github.com/ndmitchell/ghcid)、[intero](https://github.com/chrisdone/intero)などの、「IDEバックエンド」<small>（エディターが入力の補完や入力したソースコードにおけるエラーを表示する際に通信するソフトウェア）</small>は、自前でGHC APIやGHCiを呼ぶことで、型チェックしたり定義ジャンプに必要な位置情報を収集したりしていたのですが、そうした情報の収集をすべてGHC自身がHIEファイルを出力することで賄えるようになる、ということです。

私は従来開発中、`stack test --pedantic --file-watch`などとNeovimのターミナル機能で実行して実行ファイルをビルドしつつ、HIEにエラーの表示や入力の補完をさせていたのですが、その際も二重にソースコードが解析されていたんですね！  
私がそのようにわざわざ`stack test`とHIEを並行して実行させているのは、HIEがしばしばフリーズしてしまったり<small>（Neovimごと再起動すれば直ることも多いんですが...😰）</small>、HIEだけでは実行ファイルの作成やテストの実行ができない、という理由があるためです。  
`stack test`だけでHIEファイルが生成されるようになれば、エラーに関する情報やソースコードの解析結果といった情報が一元化されるので、より安定的に、より少ないリソースでHIEが使えるようになるでしょう。本家Haskell IDE Engineがサポートする日が楽しみです。

この、HIEファイルを利用するアプリケーションの例も紹介されました。  
[hie-lsp](https://github.com/wz1000/hie-lsp)という小さなLanguage Server Protocolの実装に加え、[hie-lsif](https://github.com/mpickering/hie-lsif)という、HIEファイルから「[Language Server Index Format (LSIF)](https://github.com/microsoft/language-server-protocol/blob/master/indexFormat/specification.md)」形式のファイルを作成するコマンドが印象的でした。  
このLSIFというファイルは、例えばGitHubのリポジトリ上でブラウザからソースコードを閲覧する際にも、定義ジャンプといった便利な機能を使えるようにするためのものです。リポジトリに置いたソースコードを処理系がどのように解釈したかを保存しておくことで、Language Serverはじめ処理系を実行することなく利用できるようにするものだそうです。  
現状は仕様策定中なためか、実際にLSIFを解釈するアプリケーションは見つかりませんでしたが、今後の活用に期待が高まりますね。

加えて、HIEファイルが将来的にサポートしたい機能などについても発表されました。

- 型クラスのインスタンスが、具体的にどの型のインスタンスとして解決されたかの出力
    - 定義ジャンプしたときに、型クラス自身の宣言ではなく、実装に飛べるようにするため
- 従来GHCが分割コンパイルをサポートするために、モジュールの依存情報を出力していた「インターフェースファイル<small>（`.hi`という拡張子で出力されているあのファイル）</small>」との統合
- すべての型推論の結果

# GHC status report

発表者: Simon Peyton Jones *Microsoft, UK*

GitLabへの移行やHadrianと呼ばれる新しいビルドシステムの導入など、インフラ周りでいろいろ変更があったこともあり、遅れてしまいましたがGHC 8.8がもうすぐ出るよ、という内容の発表でした<small>（発表当時。もう[GHC 8.8はリリースされています](https://www.haskell.org/ghc/blog/20190825-ghc-8.8.1-released.html)）</small>。

言及された主な追加機能は以下のとおりです。

- [`TypeApplications`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications)という言語拡張が、型変数だけでなくカインド変数に対しても適用できるようになりました。  
  正直に言って、個人的に使いどころがまだまだなさそうな機能ではありますが...。
- [`ScopedTypeVariables`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ScopedTypeVariables)という言語拡張を使った場合に、パターンマッチした変数に型注釈を付けることができるようになる、という機能がありまして、これが拡張されました。  
  具体的には、従来下記のように書くことで、関数自体の型注釈にある型変数`a`と、パターンマッチした変数`x`に型注釈した`b`が等しくなるように書くことができたのを、  

  ```haskell
  f :: forall a. Maybe a -> Int
  f (Just (x :: b)) = {- ... -}
  ```

  さらに拡張して、関数自体の型注釈にある型変数**ではない**`Int`と、パターンマッチした変数`x`に型注釈した`b`が等しくなるように書くことができるようにした、という拡張です。  
  あたかも型変数でパターンマッチしているかのようですね。

  ```haskell
  f :: Maybe Int -> Int
  f (Just (x :: b)) = {- ... -}
  ```

  一体何の役に立つの？とも思いましたが、[この修正に向けた提案](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0128-scoped-type-variables-types.rst#128motivation)曰く、

  ```haskell
  f :: ReallyReallyReallyReallyLongTypeName -> T
  f (x :: a) = … (read "" :: a) …
  ```

  と書くことで、長い型名に対して別名を付けることができるようになる、というメリットがあるそうです。なるほど💡

# とりあえず今回はここまで

テーマを絞って短い記事にした方がSEO的にいいんじゃないかと思いまして、今回は敢えて紹介する発表を絞りました。  
今後は下記のテーマについて紹介する予定です。

- HIW 2019で発表された、これからのGHCに入るであろう機能
- HIW 2019で発表された、GHC以外の言語処理系

また、HIWと同じくICFP 2019に併設して開催された、[Haskell Symposium 2019](https://icfp19.sigplan.org/home/haskellsymp-2019)の発表についても別途共有する予定です。  
乞うご期待。  
hask(\_ \_)eller
