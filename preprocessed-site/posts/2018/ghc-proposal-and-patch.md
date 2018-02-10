---
title: GHCへの変更提案とパッチ送付の手順例
headingBackgroundImage: ../../img/post-bg.jpg
headingDivClass: post-heading
subHeading: ghc-proposals, Trac ticket, Phabricator, ...
author: takenbu.hs
postedBy: takenbu.hs
date: February 12, 2018
...
---

## はじめに

Haskellのコンパイラの１つであるGHCは、オープンソースソフトウェア(OSS)のプロジェクトとして今も活発に開発が進められています。
個人の経験や経歴や肩書きや権限などに関わらず、誰でもGHCの開発にすぐに参加することができます。

ここでは、GHCに新しい変更を提案し実装するための、以下の手順例を紹介します。

1. [変更提案](#ch1) (GitHubのghc-proposals リポジトリ上にて実施)
2. [パッチ送付](#ch2) (PhabricatorのHaskell.org インスタンス上にて実施)

GHCに改善したい点があれば、誰でも変更提案が可能です。
提案のハードルは案外高いものではありません。GHC開発では、新たなcontributionが歓迎されています。  
仮に提案やパッチがreject判断されるとしても、GHCの開発者と直接やり取りする良い機会が得られます。

以下では、数値リテラルの構文を変更する単純な例をもとに、変更提案やパッチ送付の手順例を紹介します。(文章だらけになってしまいましたがご容赦を 😊 )



------

## 1. 変更提案(proposal) {#ch1}

### 概要

GHCは、コンパイラ本体やライブラリやツールチェーンなど多くの要素で構成されていますが、ここではコンパイラ本体への変更提案の手順について紹介します。

GHCのコンパイラ本体の開発では、[ユーザーに見える(user-visible)振る舞い](https://github.com/ghc-proposals/ghc-proposals#what-is-a-proposal)等を変更(追加・修正・削除など)するための提案(proposal)手順が定められています。
事前の調整や権限などを必要とせず、GitHubへのpull requestを通じて誰もが提案できます。

なお、変更提案(仕様)のプロセスと、修正パッチ送付(実装)のプロセスは、分離されています。必ずしも、変更提案者が実装まで行う必要はありません。


### 変更提案の正確な手続き

提案の具体的な手続きについては、以下に記載されています。よく読んでおきましょう。

* [https://github.com/ghc-proposals/ghc-proposals#ghc-proposals](https://github.com/ghc-proposals/ghc-proposals#ghc-proposals)

変更提案は、提案書を書いて以下の場所(リポジトリ)に、pull requestを送ることで行えます。

* [https://github.com/ghc-proposals/ghc-proposals/pulls](https://github.com/ghc-proposals/ghc-proposals/pulls)


### 変更提案のおおまかな流れ

[提案の流れ](https://github.com/ghc-proposals/ghc-proposals#what-is-the-timeline-of-a-proposal)は、ざくっと以下の通りです。

* 提案の作成
    * GitHub上で、[ghc-proposals](https://github.com/ghc-proposals/ghc-proposals)のリポジトリをforkする [(例)](https://github.com/takenobu-hs/ghc-proposals)
    * forkしてきた自分のリポジトリで作業用のブランチを作る [(例)](https://github.com/takenobu-hs/ghc-proposals/tree/numeric-underscores)
    * proposalsディレクトリの下に、"0000-プロポーザル名.rst"のファイル名で[提案用のファイルを作る](https://github.com/ghc-proposals/ghc-proposals#how-to-start-a-new-proposal) [(例)](https://github.com/takenobu-hs/ghc-proposals/blob/numeric-underscores/proposals/0000-numeric-underscores.rst)
    * "Motivation"などの[必要な項目](https://github.com/ghc-proposals/ghc-proposals#what-should-a-proposal-look-like)を、[reStructuredText](http://docs.sphinx-users.jp/rest.html)の書式に従い記述する [(例)](https://github.com/takenobu-hs/ghc-proposals/blob/numeric-underscores/proposals/0000-numeric-underscores.rst)
* 提案の送付
    * GitHub上で、ghc-proposalsのリポジトリに、pull requestを送る [(例)](https://github.com/ghc-proposals/ghc-proposals/pull/76)
    * 確定したpull requestのURLを、提案用のファイルの"This proposal is discussed at this pull request."の箇所に記載してから、再度commitし直す [(例)](https://github.com/takenobu-hs/ghc-proposals/commit/61149ee277aadc6bd46e0ad35aeb529f02da1182#diff-1128b179eb6630a402469b59a8a7dce6)
    * pull requestの Conversationのところに、"Rendered"という文字で提案ファイルへのリンクを貼っておく [(例)](https://github.com/ghc-proposals/ghc-proposals/pull/76#issue-261822915)
* 提案についての議論
    * pull request上で、[議論する](https://github.com/ghc-proposals/ghc-proposals#discussion-goals) [(例)](https://github.com/ghc-proposals/ghc-proposals/pull/76)
    * フィードバックがあれば、提案ファイルを修正する
    * 議論期間を充分に(一ヶ月くらいは)設ける
* 提案の判断
    * 議論が収束したら、[GHC Steering Committee](https://github.com/ghc-proposals/ghc-proposals#who-is-the-committee) へ、[判断依頼](https://github.com/ghc-proposals/ghc-proposals#how-to-bring-a-proposal-before-the-committee)をかける [(例)](https://github.com/ghc-proposals/ghc-proposals/pull/76#issuecomment-339952996)
    * GHC Steering CommitteeがAccepted/Rejectedを判断する [(例)](https://github.com/ghc-proposals/ghc-proposals/pull/76#event-1341434473)
    * Acceptedなら、Tracで[ticketを登録](https://ghc.haskell.org/trac/ghc/newticket?type=task)する [(例)](https://ghc.haskell.org/trac/ghc/ticket/14473)
    * 次は、コード修正パッチの作成・送付フェーズへ


### 変更提案の例

数値リテラルの構文を変更する場合の、具体的な変更提案の例を紹介します。

* [変更提案の初版](https://github.com/takenobu-hs/ghc-proposals/blob/numeric-underscores/proposals/0000-numeric-underscores.rst) | [最終的な変更提案](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0009-numeric-underscores.rst)
* [議論フェーズの例](https://github.com/ghc-proposals/ghc-proposals/pull/76)

その他の提案の例は以下にたくさんあります。

* [Open中の提案](https://github.com/ghc-proposals/ghc-proposals/pulls?q=is%3Aopen+is%3Apr)
* [Close済みの提案](https://github.com/ghc-proposals/ghc-proposals/pulls?q=is%3Apr+is%3Aclosed)


### いくつかのポイントなど

* 他の良い提案が参考になります (同じ種類の提案や議論がうまく進んでいる提案などから、色々な観点を学べます。)
* 数カ月単位で気長に根気よくやる(開発者は全員がボランティアで忙しい。)
* 提案してよいか迷う場合は、事前に[ghc-devsのML(メーリングリスト)](https://mail.haskell.org/cgi-bin/mailman/listinfo/ghc-devs)などで相談してもよい
* 英語の精度を必要以上に気にする必要はない
  日本語でしっかり考える。あとは短い文に区切って、Google翻訳にでも。

提案プロセスはGitHub上で行うものです。操作ミスがあったところでやり直しは何度でも行えます。失敗やミスを不必要に怖れる必要はありません。  
また、多くの提案はAcceptedに至らないこともあるので、結果を恥ずかしがる必要もありません。提案の結果に関わらず、提案とその議論自体が、他の開発者に新たな観点や気づき・刺激を提供できます。

それでは、提案プロセスをお楽しみ！



------

## 2. パッチ送付(patch) {#ch2}

### 概要

GHCへの変更提案に対するコード修正は、パッチを作成して送付することにより行われます。
ここでは、コード開発ツールであるPhabricatorのdifferential機能を用いる、標準的なパッチ送付の手順について紹介します。

なお、修正パッチはGitHubのpull requestを通じても送付できますが、後のコードレビューのフェーズを考慮すると、Phabricatorを用いるこの手順が効率的です。


### パッチ送付の正確な手続き

パッチ作成から送付についての具体的な手続きについては以下に記載されています。

* [How to contribute a patch to GHC](https://ghc.haskell.org/trac/ghc/wiki/WorkingConventions/FixingBugs)
* [Using Phabricator for GHC development](https://ghc.haskell.org/trac/ghc/wiki/Phabricator)

また、Phabricatorの詳細な操作手順については、以下に解説記事があります。

* [Contributing to GHC via Phabricator](https://medium.com/@zw3rk/contributing-to-ghc-290653b63147)


### パッチ送付のおおまかな流れ

パッチ送付の流れは、ざくっと以下の通りです。

* パッチの作成
    * GHCのbuild/validate用環境を[整えておく](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation)
    * GHCのリポジトリを[cloneする](https://ghc.haskell.org/trac/ghc/wiki/Building/GettingTheSources#CloningHEAD)
    * 事前にmasterブランチ上で、[buildできることを確認しておく](https://ghc.haskell.org/trac/ghc/wiki/Building/QuickStart) (master自体がfailしていることがあるため。)
    * 事前にmasterブランチ上で、[validateが正常終了することを確認しておく](https://ghc.haskell.org/trac/ghc/wiki/TestingPatches#Locally) (master自体がfailしていることがあるため。)
    * 作業用のブランチを作り、コードを修正する
        * 修正が１つのcommitにまとまっていると、後のarcコマンドでのパッチ送付がラクです。"git merge --squash"でまとめられます。
    * [テストケースを追加する](https://ghc.haskell.org/trac/ghc/wiki/Building/RunningTests/Adding) [(例)](https://phabricator.haskell.org/D4235#change-AHneoV84zpis)
    * 必要に応じて[ユーザーガイド](https://ghc.haskell.org/trac/ghc/wiki/Commentary/UserManual)に変更機能についての説明を追加する [(例)](https://phabricator.haskell.org/D4235#change-0p_6dVtsoCP3)
    * 修正コードにてbuildできることを確認しておく(必ず行う)
    * 修正コードにてvalidateが正常終了することを確認しておく(必ず行う)
* パッチの送付
    * Phabricator用のコマンドラインツール[Arcanistをインストールする](https://ghc.haskell.org/trac/ghc/wiki/Phabricator#TheCLI:Arcanist) ([arcanistツールの説明](https://secure.phabricator.com/book/phabricator/article/arcanist/))
    * [Phabricatorにパッチを送付する](https://ghc.haskell.org/trac/ghc/wiki/Phabricator#Startingoff:Fixingabugsubmittingareview) [(例)](https://phabricator.haskell.org/D4235)
        * 具体的なコマンドは"arc diff HEAD~"。 最後のcommitが送信される。
    * Tracのticketの、"Differential Rev"の箇所にPhabの管理番号を書いておく [(例)](https://ghc.haskell.org/trac/ghc/ticket/14473)
    * Phabricator上で、コードレビューしてもらう（待つ、議論する）
    * 必要に応じてコードを修正する
        * コード修正後に、修正パッチを送り直すコマンドは"arc diff"。
        * レビュー待ちの間に、masterとconflictを起こした場合は、パッチを送り直すと親切。
        * レビュー待ちの間に、masterとの差分が大きくなった場合は、"git rebase"してから送り直すのも親切。rebaseについては[ここを参照](https://ghc.haskell.org/trac/ghc/wiki/Phabricator#Workingwithmultipledependentdiffs)
    * レビューが完了してmasterブランチに取り込まれたら、proposalsの"implemented"のフィールドに、実装済みのGHCのバージョン番号を記載しておく [(例)](https://github.com/ghc-proposals/ghc-proposals/commit/1974c2a45a782461084ea596ec839638d4ff0743#diff-ffb9f607b8f1e69494a276ae9afa8268)


### パッチ送付の例

数値リテラルの構文を変更する場合の、具体的なパッチ送付の例を紹介します。

* [https://phabricator.haskell.org/D4235](https://phabricator.haskell.org/D4235)

その他のレビュー中パッチの例は以下にたくさんあります。

* [https://phabricator.haskell.org/differential/](https://phabricator.haskell.org/differential/)



### いくつかのポイントなど

* 他の良いパッチが参考になります(同じ種類の修正を探すと、修正方法や慣習や修正漏れなどを確認できます。)
* build確認とvalidation確認は絶対に行う(つたないコードは問題視されませんが、本来行うべき手順を行わないことは、開発全体にダメージを与えるとともに、個人の信用度に影響します。)
* 数カ月単位で気長に根気よくやる(パッチ作業は多数並走しており、GHCのリリース時期は特に多忙です。全員がボランティアで行っている自発的な活動ですので、忘れられている状況へのpingは構いませんが、強い催促は控えるのが賢明です。)
* わからない点は、ghc-devs MLやPhabricator上で相談するとよいでしょう。
* Phabricator(arcコマンド)には慣れが必要かと思います。最初は影響範囲の少ない、ドキュメント修正などでPhabricatorの作業手順に慣れていくのも良いです。

パッチ送付は、Phabricatorやgitの機能を用いて行うものです。操作ミスがあったところで、GHCのリポジトリ本体に直ちに反映されるわけではありません。やり直しは何度でも行えます。失敗やミスを不必要に怖れる必要はありません。communityのためになるcontributionは常に歓迎されています。

それでは、パッチ送付プロセスをお楽しみ！



------

## 補足

わからないことがあれば、[ghc-devsのML](https://mail.haskell.org/cgi-bin/mailman/listinfo/ghc-devs)に問い合わせると親切に教えてもらえます。
もちろん、[Haskell-jpのslack](https://join-haskell-jp-slack.herokuapp.com/)の#questionsチャネルなどで尋ねるのも良いでしょう。

なお、GHCでの開発作業については、[Working on GHC](https://ghc.haskell.org/trac/ghc/wiki/WorkingConventions)も参考にどうぞ。  
また、GHCの開発フロー全体については、[こちら](https://takenobu-hs.github.io/downloads/ghc_development_flow.pdf)も参考にどうぞ。GHC関連のサイトの情報を力づくで検索するには、[こちら](https://takenobu-hs.github.io/haskell-wiki-search/)もどうぞ。



Happy Hacking!

以上です。

