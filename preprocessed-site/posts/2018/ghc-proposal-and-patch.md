---
title: GHCへの変更提案とパッチ送付の手順例
headingBackgroundImage: ../../img/post-bg.jpg
headingDivClass: post-heading
subHeading: ghc-proposals, trac ticket, phabricator, ...
author: takenbu.hs
postedBy: takenbu.hs
date: February 12, 2018
...
---

## はじめに

Haskellのコンパイラの１つであるGHCは、オープンソースソフトウェア(OSS)のプロジェクトとして今も開発が進められています。
個人の経験や経歴や肩書きや権限などに関わらず、誰でもGHCの開発にすぐに参加することができます。

ここでは、GHCに新しい変更を提案するための、以下の手順例を紹介します。

1. [変更提案](#ch1) (githubのghc-proposals リポジトリ上にて実施)
2. [パッチ送付](#ch2) (phabricatorのHaskell.org インスタンス上にて実施)

GHCに改善したい点があれば、誰でも変更提案が可能です。
提案のハードルは案外高いものではありません。GHC開発では、新たなcontributionが歓迎されています。  
仮に提案やパッチがreject判断されるとしても、GHCの開発者と直接やり取りする良い機会が得られます。

以下では、数値リテラルの構文を変更する例をもとに、この程度の内容でも、変更提案やパッチ送付が可能という手順例を紹介します。



------

## 1. 変更提案(proposal) {#ch1}

### 概要

GHCは、コンパイラ本体やライブラリやツールチェーンなど多くの要素で構成されていますが、ここではコンパイラ本体への変更提案の手順について紹介します。

GHCのコンパイラ本体の開発では、ユーザーに見える(user-visible)振る舞いを変更(追加・修正・削除など)するための提案(proposal)手順が定められています。
事前の調整や権限などを必要とせず、Githubへのpull requestを通じて誰もが提案できます。

なお、変更提案のプロセスと、実装(修正パッチ送付)のプロセスは、分離されています。必ずしも、変更提案者が実装まで行う必要はありません。


### 変更提案の正確な手続き

提案の具体的な手続きについては以下に記載されています。よく読んでおきましょう。

* [https://github.com/ghc-proposals/ghc-proposals#ghc-proposals](https://github.com/ghc-proposals/ghc-proposals#ghc-proposals)

変更提案は、提案書を書いて以下の場所に、pull requestを送ることで行えます。

* [https://github.com/ghc-proposals/ghc-proposals/pulls](https://github.com/ghc-proposals/ghc-proposals/pulls)


### 変更提案のおおまかな流れ

提案の流れは、ざくっと以下の通りです。

* 提案の作成
    * Github上で、[ghc-proposals](https://github.com/ghc-proposals/ghc-proposals)のリポジトリをforkする
    * forkしてきた自分のリポジトリで作業用の分岐を作る
    * proposalsディレクトリの下に、0000-XXX.rstのファイル名で[提案用のファイルを作る](https://github.com/ghc-proposals/ghc-proposals#how-to-start-a-new-proposal) 
    * [必要な項目](https://github.com/ghc-proposals/ghc-proposals#what-should-a-proposal-look-like)を、reStructuredTextの書式に従い記述する
* 提案の送付
    * Github上で、pull requestを送る
    * pull requestの番号が確定するので、提案用のファイルに確定したURLを記載して、再度commitし直す
    * 自分のpull requestの Conversationのところに、"Rendered"という文字で提案ファイルへのリンクを貼っておく
* 提案の議論
    * pull request上で、[議論する](https://github.com/ghc-proposals/ghc-proposals#discussion-goals) [(例)](https://github.com/ghc-proposals/ghc-proposals/pull/76)
    * フィードバックがあれば、提案ファイルを修正する
    * 議論期間を充分に(一ヶ月くらいは)設ける
* 提案の判断
    * 議論が収束したら、GHC Steering Committee へ、[判断依頼](https://github.com/ghc-proposals/ghc-proposals#how-to-bring-a-proposal-before-the-committee)をかける [(例)](https://github.com/ghc-proposals/ghc-proposals/pull/76#issuecomment-339952996)
    * GHC Steering CommitteeがAccepted/Rejectedを判断する
    * [Accepted](https://github.com/ghc-proposals/ghc-proposals/pull/76#event-1341434473)なら、tracへticketを登録する [(例)](https://ghc.haskell.org/trac/ghc/ticket/14473)
    * 次は、コード修正パッチの作成・送付フェーズへ


### 変更提案の例

数値リテラルの構文を変更する場合の、具体的な変更提案の例を紹介します。

* [変更提案の初版](https://github.com/takenobu-hs/ghc-proposals/blob/numeric-underscores/proposals/0000-numeric-underscores.rst) | [最終的な変更提案](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0009-numeric-underscores.rst)
* [議論フェーズの例](https://github.com/ghc-proposals/ghc-proposals/pull/76)

その他の提案の例は以下にたくさんあります。

* [Open中の提案](https://github.com/ghc-proposals/ghc-proposals/pulls?q=is%3Aopen+is%3Apr)
* [Close済みの提案](https://github.com/ghc-proposals/ghc-proposals/pulls?q=is%3Apr+is%3Aclosed)


### いくつかのポイントなど

* 他の良い提案を参考にする (同じ種類の提案や議論がうまく進んでいる提案など)
* 数カ月単位で気長に根気よくやる(全員がボランティアで忙しい)
* 提案してよいか迷う場合は、事前に[ghc-devsのML(メーリングリスト)](https://mail.haskell.org/cgi-bin/mailman/listinfo/ghc-devs)などで相談してもよい
* 英語の精度を必要以上に気にする必要はない
  日本語でしっかり考える。あとは短い文に区切って、Google翻訳にでも。

提案プロセスはGithub上で行うものです。操作ミスがあったところでやり直しは何度でも行えます。失敗やミスを不必要に怖れる必要はありません。また、多くの提案はAcceptedに至らないこともあるので、結果を恥ずかしがる必要もありません。

それでは、提案プロセスをお楽しみ！



------

## 2. パッチ送付(patch) {#ch2}

### 概要

GHCへの提案済みの変更やバグ修正は、修正パッチを作成して送付することにより行われます。
ここでは、コードレビューツールであるPhabricatorのdifferential機能を用いる標準的な手順について紹介します。

なお、修正パッチはGithubのpull requestを通じても送付できますが、後のコードレビューを考慮すると、Phabricatorを用いるこの標準的な手順が効率的です。


### パッチ送付の正確な手続き

パッチ作成から送付についての具体的な手続きについては以下に記載されています。

* [How to contribute a patch to GHC](https://ghc.haskell.org/trac/ghc/wiki/WorkingConventions/FixingBugs)
* [Using Phabricator for GHC development](https://ghc.haskell.org/trac/ghc/wiki/Phabricator)

また、Phabricatorの使い方については、以下に解説記事があります。

* [Contributing to GHC via Phabricator](https://medium.com/@zw3rk/contributing-to-ghc-290653b63147)


### パッチ送付のおおまかな流れ

パッチ送付の流れは、ざくっと以下の通りです。

* パッチの作成
    * GHCのbuild/validate用環境を[整えておく](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation)
    * GHCのリポジトリを[cloneする](https://ghc.haskell.org/trac/ghc/wiki/Building/GettingTheSources#CloningHEAD)
    * 事前にmaster分岐上で、[buildできることを確認しておく](https://ghc.haskell.org/trac/ghc/wiki/Building/QuickStart) (master自体がfailしていることがあるため。)
    * 事前にmaster分岐上で、[validateが正常終了することを確認しておく](https://ghc.haskell.org/trac/ghc/wiki/TestingPatches#Locally) (master自体がfailしていることがあるため。)
    * 作業用の分岐を作り、コードを修正する
        * 最終的に、修正が１つのcommitにまとまっていると、後のarcコマンドでのパッチ送付がラク。"git merge --squash"でまとめられる。
    * [テストを追加する](https://ghc.haskell.org/trac/ghc/wiki/Building/RunningTests/Adding) [(例)](https://phabricator.haskell.org/D4235#change-AHneoV84zpis)
    * 必要に応じて[ユーザーガイド](https://ghc.haskell.org/trac/ghc/wiki/Commentary/UserManual)に機能の説明を追加する [(例)](https://phabricator.haskell.org/D4235#change-0p_6dVtsoCP3)
    * 修正コードにてbuildできることを確認しておく(必ず行う)
    * 修正コードにてvalidateが正常終了することを確認しておく(必ず行う)
* パッチの送付
    * Phabricator用のツール[Arcanistをインストールする](https://ghc.haskell.org/trac/ghc/wiki/Phabricator#TheCLI:Arcanist) ([arcanist自体の説明](https://secure.phabricator.com/book/phabricator/article/arcanist/))
    * [Phabricatorにパッチを送付する](https://ghc.haskell.org/trac/ghc/wiki/Phabricator#Startingoff:Fixingabugsubmittingareview) 
        * 具体的なコマンドは"arc diff HEAD~"。 最後のcommitが送信される。
    * trac ticketの、"Differential Rev"の箇所にPhabの管理番号を書いておく [(例)](https://ghc.haskell.org/trac/ghc/ticket/14473)
    * コードレビューしてもらう（待つ、議論する）
    * 必要に応じて修正する
        * コード修正後に、修正パッチを送り直すコマンドは"arc diff"。
        * レビュー待ちの間に、masterとconflictを起こした場合は、パッチを送り直すと親切。
        * レビュー待ちの間に、masterとの差分が大きくなった場合は、"git rebase"してから送り直すのも親切。rebaseについては[ここを参照](https://ghc.haskell.org/trac/ghc/wiki/Phabricator#Workingwithmultipledependentdiffs)
    * レビューが完了してmaster分岐に取り込まれたら、proposalsの"implemented"のフィールドに、実装済みのghcのバージョン番号を記載しておく [(例)](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0009-numeric-underscores.rst)


### パッチ送付の例

数値リテラルの構文を変更する場合の、具体的なパッチ送付の例を紹介します。

* [https://phabricator.haskell.org/D4235](https://phabricator.haskell.org/D4235)

その他のレビュー中パッチの例は以下にたくさんあります。

* [https://phabricator.haskell.org/differential/](https://phabricator.haskell.org/differential/)



### いくつかのポイントなど

* 他の良いパッチを参考にする(同じ種類の修正で、修正方法や修正漏れの確認に)
* build確認とvalidation確認は絶対に行う。(つたないコードは問題視されませんが、本来行うべき手順を行わないことは個人への信用度に影響します。)
* 数カ月単位で気長に根気よくやる。(パッチ作業は多数並走しており、GHCのリリース時期は特に多忙です。全員がボランティアで行っている自発的な活動ですので、忘れられている状況へのpingは構いませんが、強い催促は控えるのが賢明です。)
* わからない点は、ghc-devs MLやPhabricator上で相談する
* Phabricator(arcコマンド)には最初慣れが必要かと思います。最初は影響範囲の少ない、ドキュメント修正などで手順に慣れていくのも良いです。

パッチの送付は、Phabricatorやgitの機能を用いて行うものです。操作ミスがあったところで、ghcのリポジトリ本体に直ちに反映されるわけではありません。やり直しは何度でも行えます。失敗やミスを不必要に怖れる必要はありません。

それでは、パッチ送付プロセスをお楽しみ！



------

## 補足

わからないことがあれば、[ghc-devsのML](https://mail.haskell.org/cgi-bin/mailman/listinfo/ghc-devs)で教えてもらえます。
もちろん、[Haskell-jpのslack](https://join-haskell-jp-slack.herokuapp.com/)の#questionsチャネルなどで尋ねるのも良いでしょう。

なお、GHCでの開発作業のここについては、[Working on GHC](https://ghc.haskell.org/trac/ghc/wiki/WorkingConventions)も参考にどうぞ。  
また、GHCの開発フロー全体については、[こちら](https://takenobu-hs.github.io/downloads/ghc_development_flow.pdf)も参考にどうぞ。GHC関連のサイトの情報を力づくで検索するには、[こちら](https://takenobu-hs.github.io/haskell-wiki-search/)もどうぞ。



Happy Hacking!

以上です。

