---
title: Haskell Advent Calendar 2017 まとめ
headingBackgroundImage: ../../img/post-bg.jpg
headingDivClass: post-heading
subHeading: 2017年の AdC に投稿された Haskell 記事のリンク集
author: MATSUBARA Nobutada
postedBy: <a href="https://matsubara0507.github.io/">MATSUBARA Nobutada(@matsubara0507)</a>
date: December 31, 2017
tags: Links
...
---

これは [Haskell Advent Calendar その4](https://qiita.com/advent-calendar/2017/haskell4) の12日目の記事です。

なんで12日目の記事で「まとめ」をやっているのかって？
25日を過ぎてから余ってた日付に登録しただけですよ。


[Qiita](https://qiita.com/advent-calendar/2017/calendars) と [ADVENTAR](https://adventar.org/calendars?year=2017)  のアドベントカレンダーで投稿された、Haskell に関する記事を集めてみました。
かなり雑ですが機械的に集めたので、それなりに拾えてると思いますが、もし「この記事が無いよ」とか、逆に 「Haskell ちゃうやんこの記事」ってのがあったら PR でも送ってください。

ちなみに「[Elm Advent Calendar 2017 まとめ](https://scrapbox.io/miyamoen/Elm_Advent_Calendar_2017_%E3%81%BE%E3%81%A8%E3%82%81)」という記事が面白かったので、その Haskell 版オマージュ(パクリ)です。

## Haskell Advent Calendar

Qiita の方では4つもできましたね。

- [Haskell Advent Calendar 2017 - Qiita](https://qiita.com/advent-calendar/2017/haskell)
- [Haskell (その2) Advent Calendar 2017 - Qiita](https://qiita.com/advent-calendar/2017/haskell2)
- [Haskell (その3) Advent Calendar 2017 - Qiita](https://qiita.com/advent-calendar/2017/haskell3)
- [Haskell (その4) Advent Calendar 2017 - Qiita](https://qiita.com/advent-calendar/2017/haskell4)

まぁしかし、残念ながらどのカレンダーも埋まってないため[ランキング](https://qiita.com/advent-calendar/2017/ranking/subscriptions)圏外となってしまいましたが(笑)

加えて何故か、25日が過ぎてからその5のカレンダーができるという、なかなか面白い事案が発生しました。

- [Haskell (その5) Advent Calendar 2017 - Qiita](https://qiita.com/advent-calendar/2017/haskell5)

上記のカレンダーの記事以外も含めて、全部で102記事もありました。
みんなすごいですね。

## 記事を集めた

超雑にスクレイパーを書いて Qiita と ADVENTAR のカレンダーをスクレイピングしてきました。
プログラムは以下のリポジトリにあります(もちろん Haskell で作ったよ)。

- [matsubara0507/advent-calendar - GitHub](https://github.com/matsubara0507/haskell-advent-calendar)

カレンダーのタイトルか、記事のタイトルに「Haskell」って単語が入っているやつだけ集めてます。
このプログラムの解説っぽい記事はそのうち自分のとこの記事として挙げる気がする(たぶん)。

## 記事たち

分類は温もりのある手作業でやってます。
自然言語処理系が出来れば機械的に分類できたかもしれませんが...

分類違くね？というモノがあれば PR でも送ってください。

それでは良いお年を。

### ポエム

**[なぜ Haskell が好きなのか - 趣味はデバッグ……](http://kakkun61.hatenablog.com/entry/2017/12/25/%E3%81%AA%E3%81%9C_Haskell_%E3%81%8C%E5%A5%BD%E3%81%8D%E3%81%AA%E3%81%AE%E3%81%8B)**  
 by kakkun61 on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/06

**[Haskell副作用ポエム - Qiita](https://qiita.com/Mizunashi_Mana/items/e82214dfae2765c6839a)**  
 by Mizunashi_Mana on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/21

**[Haskellを勉強して感動したこと・難しいと思ってること - Qiita](https://qiita.com/ababup1192/items/b15cdace30b7fef3338c)**  
 by ababup1192 on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/04

### 入門してみた

**[Haskell入門しようとして環境構築で失敗。 · GitHub](https://gist.github.com/sys9kdr/2484ed09f5cb7ea86beae8f5222d9a3c)**  
 by sys9kdr on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/05

**[ClojurianがHaskellでWeb API開発に入門してみた - Qiita](https://qiita.com/lagenorhynque/items/f8b14ff70a26cfd27976)**  
 by lagenorhynque on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/19

**[美術の人が考える Haskell - Qiita](https://qiita.com/hitsujisanmeme/items/e14972cfd349c1149d58)**  
 by hitsujisanmeme on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/14

**[Haskell入門者がライブラリを触っちゃう!? - Qiita](https://qiita.com/brackss1/items/9f9466f160391dc53bba)**  
 by brackss1 on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/22

**[Ubuntu、Haskellでwebアプリ手始め - Qiita](https://qiita.com/ryosukue/items/cf14d817a7067dfd2094)**  
 by ryosukue on [Nuco Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/nuco) 12/23

**[Haskellをちょこっと紹介（フィボナッチ数列を書いてみる） - Qiita](https://qiita.com/3nan/items/b89b7383ddb2bac5dda1)**  
 by 3nan on [TECOTEC Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/tecotec) 12/23

### ノウハウ

**[私のHaskellコーディングスタイルガイド,改行出来るポイントを紹介 - ncaq](https://www.ncaq.net/2017/12/02/00/00/00/)**  
 by ncaq on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/02

**[ゲーム販売webアプリケーションSYAKERAKEを支える技術,HaskellとYesodで作られています - ncaq](https://www.ncaq.net/2017/12/03/00/00/00/)**  
 by ncaq on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/03

**[Stackage Serverのちょっとした便利な使い方,Hoogleをブラウザのカスタム検索エンジンに追加しましょう,よく使うパッケージをブックマークする時はPackageRのURLにしましょう - ncaq](https://www.ncaq.net/2017/12/04/00/00/00/)**  
 by ncaq on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/04

**[goな関数](http://d.hatena.ne.jp/kazu-yamamoto/20171212/1513050147)**  
 by kazu_yamamoto on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/01

**[HaskellのABC(Haskell Advent Calendar 6th) - モナドとわたしとコモナド](http://fumieval.hatenablog.com/entry/2017/12/19/203500)**  
 by fumieval on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/06

**[WindowsでHaskellを扱う時によく遭遇するエラーと対処法 - Haskell-jp](https://haskell.jp/blog/posts/2017/windows-gotchas.html)**  
 by igrep on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/14

### 解説

**[A Tour of Go in Haskellを作ったのと、GoとHaskellの比較 - syocy’s diary](http://syocy.hatenablog.com/entry/a-tour-of-go-in-haskell)**  
 by syocy on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/03

**[ServantのハンドラにReaderTを適用する - Qiita](https://qiita.com/cyclone_t/items/8443ed5d4a77f87b1f1b)**  
 by cyclone_t on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/15

**[スーパーモナドについて - Qiita](https://qiita.com/wgag/items/11a6e667011d530832e8)**  
 by wgag on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/10

**[CircleCI2.0でHaskellのテストを実行する – PSYENCE:MEDIA](https://tech.recruit-mp.co.jp/dev-tools/post-13981/)**  
 by yukiasai on [RECRUIT MARKETING PARTNERS Advent Calendar 2017 - Adventar](http://adventar.org/calendars/2360) 12/13

**[Dokku環境を構築してHaskellのアプリケーションをデプロイする - Qiita](https://qiita.com/yukiasai/items/248294a871d40c0dcef6)**  
 by yukiasai on [Recruit Engineers  Advent Calendar 2017 - Adventar](http://adventar.org/calendars/2324) 12/07

### 周辺ツールについて

**[Haskell Stack とは何をするツールなのか - Qiita](https://qiita.com/usamik26/items/672ed3c4451402bfc275)**  
 by usamik26 on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/12

**[stack ls コマンドが追加されます](https://haskell.e-bigmoon.com/posts/2017-12-20-stack-ls-command.html)**  
 by waddlaw on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/08

**[stack v1.6.3 がリリースされました。](https://haskell.e-bigmoon.com/posts/2017-12-24-stack163.html)**  
 by waddlaw on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/09

**[Haskellや周辺ツールについてのリンク集 - Qiita](https://qiita.com/ogata-k/items/23d70250ec42359b6bb5)**  
 by ogata-k on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/15

**[Haskellのstackによるプロジェクトについて - Qiita](https://qiita.com/ogata-k/items/2b21326b2b7351bfc28c)**  
 by ogata-k on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/16

**[haddock に Grid Table 記法が追加されました](https://haskell.e-bigmoon.com/posts/2017-12-27-haddock-grid-table.html)**  
 by waddlaw on [Haskell (その5) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell5) 12/02

**[カスタムスナップショットの紹介](https://haskell.e-bigmoon.com/posts/2017-12-23-stack161.html)**  
 by waddlaw on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/14

**[Haskellプロジェクトを始めるにあたって - The curse of λ](https://myuon.github.io/posts/haskell-project-setup/)**  
 by myuon_myon on [一人Computer Science Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/myuon_myon_cs) 12/15

### ライブラリ紹介

**[servant+persistentを利用する - Qiita](https://qiita.com/jabaraster/items/e8ebbe6d25b535947aba)**  
 by jabaraster on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/07

**[YampaでFunctional Reactiveな認知行動療法ボット - Qiita](https://qiita.com/makoraru/items/596729de09d6aeb81e5a)**  
 by makoraru on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/13

**[Haskellで機械学習を実装しようと思った過去の自分へ](https://nnwww.github.io/blog/post/haskell_ml/)**  
 by \_Nnwww on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/23

**[Haskell・Servant+Persistent/Esqueletoで作る実用WebAPI (1) Servantの基本 - Qiita](https://qiita.com/cyclone_t/items/52ad44cfbb4603e123f3)**  
 by cyclone_t on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/11

**[Extensible Effects ステップ・バイ・ステップ](https://matsubara0507.github.io/posts/2017-12-09-extensible-effects-step-by-step.html)**  
 by matsubara0507 on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/09

**[Haskellのパーサライブラリまとめ - Qiita](https://qiita.com/Mizunashi_Mana/items/115855bf2af9b9970198)**  
 by Mizunashi_Mana on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/11

**[Haskell入門者LT会で自作ライブラリnetwork-voicetextの話をしてきた | ザネリは列車を見送った](https://www.zaneli.com/blog/20171213)**  
 by zaneli@github on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/13

**[Haskell と SQLite - Qiita](https://qiita.com/satosystems/items/32bf104a041c8cc13809)**  
 by satosystems on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/16

**[部分構造の共有を観測するdata-reify - Qiita](https://qiita.com/masahiro_sakai/items/6a989d5b898a08f17821)**  
 by masahiro_sakai on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/21

### 作ってみた

**[Elm と Haskell で作る ToDo アプリ](https://matsubara0507.github.io/posts/2017-12-13-elm-and-haskell-for-elmer.html)**  
 by matsubara0507 on [Elm Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/elm) 12/13

**[Haskellに入門して1年位経ったのでライフゲームを作ってみた話 - abc10946の日記](http://abc10946.hatenablog.com/entry/2017/12/18/014203)**  
 by ABC10946 on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/18

**[仕事でHaskellを使いたいなら外堀から](http://d.hatena.ne.jp/wvogel00/20171222/1513924995)**  
 by hxf_vogel on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/22

**[slack-api + bloodhound + servant でbot+αを作る](http://blog.nakaji.me/slack-api-bloodhound-servant/)**  
 by nakaji-dayo on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/07

**[【Haskell (その2) Advent Calendar 2017】Vim から Hoogle の検索が出来るプラグインをつくった【16日目】 - Secret Garden(Instrumental)](http://secret-garden.hatenablog.com/entry/2017/12/16/000000)**  
 by pink_bangbi on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/16

**[お天気Bot で理解する Haskell の便利パッケージ - Qiita](https://qiita.com/rounddelta/items/807866bdaa81c3057ac0)**  
 by rounddelta on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/25

**[Haskell ochintin-daicho で年末調整プログラミング - Qiita](https://qiita.com/arowM/items/e59442120ad3c1071c57)**  
 by arowM on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/12

**[作って学ぶBitcoin！ゼロから作るSPVウォレット - Qiita](https://qiita.com/lotz/items/1aa6cf18aa193f40c647)**  
 by lotz on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/18

**[優秀な秘書を雇いました!!! - Creatable a => a -> IO b](http://tune.hateblo.jp/entry/2017/12/27/031803)**  
 by tokiwoousaka@github on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/20

**[Haskell における遅延ファイル読み込みとリソースリーク - Qiita](https://qiita.com/satosystems/items/c1c0feef87345a9df69d)**  
 by satosystems on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/23

**[タイプセーフプリキュア！を支える技術 その2 - Haskell-jp](https://haskell.jp/blog/posts/2017/typesafe-precure2.html)**  
 by igrep on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/24

**[Haskellによるwebスクレイピングの方法をdic-nico-intersection-pixivを例に書く - ncaq](https://www.ncaq.net/2017/12/19/00/00/00/)**  
 by エヌユル on [Webスクレイピング Advent Calendar 2017 - Adventar](http://adventar.org/calendars/2105) 12/19

**[Re: ゼロから作る ADVENTAR の Slack Bot (Haskell 編)](https://matsubara0507.github.io/posts/2017-12-02-re-adventar-slack-bot-part1.html)**  
 by ひげ on [群馬大学電子計算機研究会 IGGG Advent Calendar 2017 - Adventar](http://adventar.org/calendars/2300) 12/02

### 処理系

**[GHCの中間言語Coreへの脱糖を覗き見る - Hash λ Bye](http://ilyaletre.hatenablog.com/entry/2017/12/10/195016)**  
 by ilyaletre on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/11

**[Haskellの型システムを書く(1) - 純粋技術メモ](http://fujiy.hatenablog.com/entry/type-inference-1)**  
 by fujiy on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/03

**[GHCでの中置演算子のパース - Qiita](https://qiita.com/takoeight0821/items/9a1c3eb5b0f292026596)**  
 by takoeight0821 on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/06

**[型システムを学ぼう！](https://uhideyuki.sakura.ne.jp/studs/index.cgi/ja/HindleyMilnerInHaskell)**  
 by unnohideyuki on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/15

### 言語機能

**[Haskell Backpack 覚え書き](https://matsubara0507.github.io/posts/2017-12-12-backpack-memo.html)**  
 by matsubara0507 on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/12

**[にこ、希と一緒に学ぶHaskell（番外）「あまり知られていないGHC拡張の紹介」 - Qiita](https://qiita.com/aiya000/items/b802531c58c161cd245f)**  
 by aiya000 on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/21

**[Levity polymorphismについて軽く - Qiita](https://qiita.com/ruicc/items/e2879c44eba77b1e7170)**  
 by ruicc on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/02

**[Kindについて - Qiita](https://qiita.com/ryoppy/items/7156d587da2e6ae7e605)**  
 by ryoppy on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/11

**[OverloadedLabels と Haskell Relational Record - khibino blog](http://khibino.hatenadiary.jp/entry/2017/12/18/081814)**  
 by khibino on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/18

**[GHC 8.2 以前で FFI を使う際に注意すること - Qiita](https://qiita.com/thimura/items/0d289c231f9aceac61dc)**  
 by thimura on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/19

### 型

**[依存型の紹介と応用としてのClashの紹介 - Qiita](https://qiita.com/junjihashimoto@github/items/31f245f5e0138e5fac7e)**  
 by junjihashimoto@github on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/19

**[ことり、穂乃果と一緒に学ぶHaskell（入門）その6「高階データ型」 - Qiita](https://qiita.com/aiya000/items/81e8424c8a3ee23586a6)**  
 by aiya000 on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/24

**[型を実行時に作る：怖くないリフレクション - Qiita](https://qiita.com/mod_poppo/items/50ad2c0ee66171cc1ee9)**  
 by mod_poppo on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/19

**[Haskellにおける型クラス制約の役割 - Qiita](https://qiita.com/HirotoShioi/items/5474119ba3682448c109)**  
 by HirotoShioi on [Haskell (その5) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell5) 12/04

### Base

**[HaskellのStateの必要性が,プログラミング言語の処理系を書いた時にわかったので,Stateの良さを語ります - ncaq](https://www.ncaq.net/2017/12/01/00/00/01/)**  
 by ncaq on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/01

**[Haskell - $の仕組みを覗いてみよう - Qiita](https://qiita.com/grainrigi/items/f45b586b4013ffc3814e)**  
 by grainrigi on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/08

**[Mapping things](https://blog.b123400.net/functor/)**  
 by b123400 on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/10

**[Maybe自作から学ぶHaskell！ - Qiita](https://qiita.com/elipmoc101/items/4590210c9946ee2fd4c5)**  
 by elipmoc101 on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/16

**[Listで遊ぶ | 慶應義塾大学ロボット技術研究会](https://keiorogiken.wordpress.com/2017/12/24/list%E3%81%A7%E9%81%8A%E3%81%B6/)**  
 by mt_caret on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/20

**[Arrowを理解する - Qiita](https://qiita.com/Lugendre/items/6b4a8c8a9c85fcdcb292)**  
 by Lugendre on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/08

### アルゴリズムとデータ構造

**[探索問題を Haskell で解く - Qiita](https://qiita.com/ryohji/items/d8bdba1648978c308cdd)**  
 by ryohji on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/09

**[巡回セールスマン問題を遺伝的アルゴリズムとデータ構造を使ってHaskellで解く！ - Qiita](https://qiita.com/v97ug/items/d6dd50a2b6b84a9e4d41)**  
 by v97ug on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/15

**[キューの効率的な実装 または私は如何にしてHaskellを止めてF#を愛するようになったか - Qiita](https://qiita.com/rst76/items/a7dd81b522a09d1b9986)**  
 by rst76 on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/21

**[永続リアルタイムキューのHaskell実装と計算量解析 - autotaker's blog](http://autotaker.hatenablog.com/entry/2017/12/21/125153)**  
 by autotaker1984 on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/22

**[Zipperに挑む - Qiita](https://qiita.com/Aruneko/items/dee20161358e7c39e27a)**  
 by Aruneko on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/02

**[Tree: 親子関係の付け替え - Qiita](https://qiita.com/nobsun/items/27fe53516cbb90ba02e2)**  
 by nobsun on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/07

**[コラッツの問題をHaskellで書いてみた - Zodiacの黙示録](http://zodi-g12.hatenablog.com/entry/2017/12/06/133951)**  
 by zodi_G12 on [IQが1 Advent Calendar 2017 - Adventar](http://adventar.org/calendars/2377) 12/06

### 数学・科学

**[[Haskell] とびだせ！Hask圏 - Qiita](https://qiita.com/tezca686/items/855236ccdda584ee8ebb)**  
 by tezca686 on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/14

**[区間代数と無限小と無限大 - Qiita](https://qiita.com/makoraru/items/768089b2aab1dde47593)**  
 by makoraru on [Haskell  (その3) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell3) 12/17

**[しりとりの圏の回答、または定理証明Haskellを少しだけ - Qiita](https://qiita.com/as_capabl/items/2031fe28e577e77dc269)**  
 by as_capabl on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/04

**[しりとりの圏の実装(未完) - Qiita](https://qiita.com/hiratara/items/6265b5d4791144bee33b)**  
 by hiratara on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/05

**[量子コンピューターにはモナドがよく似合う - Qiita](https://qiita.com/kyamaz/items/67ec5c7d39e62c1de91d)**  
 by kyamaz on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/17

**[Haskell上で有限体を使って遊ぶ - Qiita](https://qiita.com/NaOHaq/items/ba490cc1e1ab890cb399)**  
 by NaOHaq on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/22

**[somehow-morphisms on fixed point written in Haskell - Qiita](https://qiita.com/cutsea110/items/69889a99d30b627bc04a)**  
 by cutsea110 on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/01

**[量子プログラミングはHaskellで - Qiita](https://qiita.com/kyamaz/items/e49b07e05e871a22246c)**  
 by kyamaz on [量子コンピュータ Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/quantum) 12/17

### Docker

**[HaskellでもGoみたいにシングルバイナリでアプリケーションをデプロイしたい - Qiita](https://qiita.com/t10471/items/4afa598e1be5d6c7cc1f)**  
 by t10471 on [Haskell (その2) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell2) 12/09

**[Haskell on Docker で Portable CLI を作ろう - Qiita](https://qiita.com/algas/items/fde155abbc9d8ae3f8c9)**  
 by algas on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/05

### Eta

**[Etaで表現されるデータ型としてのJavaクラスとその継承関係 - Qiita](https://qiita.com/aiya000/items/881d5f7e04b1178e7764)**  
 by aiya000 on [Haskell Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell) 12/25

**[Haskell (Eta) でJavaFXのEDSLを作る雰囲気を醸し出す - Qiita](https://qiita.com/aiya000/items/7dc0cb8694de0675088e)**  
 by aiya000 on [プロ生ちゃん Advent Calendar 2017 - Adventar](http://adventar.org/calendars/2546) 12/25

### 書籍紹介

**[これから Haskell を学ぶ人のための書籍紹介 - Qiita](https://qiita.com/waddlaw/items/dd926462d398c4cbd019)**  
 by waddlaw on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/01

**[2017年に「すごいHaskellたのしく学ぼう」を読む - Qiita](https://qiita.com/Aruneko/items/e72f7c6ee49159751cba)**  
 by Aruneko on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/13

**[拙書「Haskell 教養としての関数型プログラミング」の紹介 - Qiita](https://qiita.com/YoshikuniJujo/items/0708f108bf53a216a61a)**  
 by YoshikuniJujo on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/24

**[mt_caret.log - 本1 "Haskell Programming from first principles"](https://mt-caret.github.io/blog/posts/2017-12-01-book01.html)**  
 by mt_caret on [本 Advent Calendar 2017 - Adventar](http://adventar.org/calendars/2433) 12/01

### 翻訳記事

**[Haskell で暗号学的ハッシュを扱う (和訳) - Qiita](https://qiita.com/Pythonissam/items/ace180b1b78876f1c190)**  
 by rounddelta on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/02

**[Haskell のアプリケーション向けに軽量の Dockerイメージ を作る (和訳) - Qiita](https://qiita.com/rounddelta/items/d767f36d544427a9c60d)**  
 by rounddelta on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/03

**[Haskell 初心者へのアドバイス (和訳) - Qiita](https://qiita.com/rounddelta/items/8b7d2a200a932e761fa3)**  
 by rounddelta on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/04

**[将来も使えるテストスイート (和訳) - Qiita](https://qiita.com/rounddelta/items/31b835b493abf5be3549)**  
 by rounddelta on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/05

**[VS Code で Ghcid を使う (和訳) - Qiita](https://qiita.com/rounddelta/items/27c12237d9ef1c5569ba)**  
 by rounddelta on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/06

**[HLint のルールを理解する (和訳) - Qiita](https://qiita.com/rounddelta/items/4584f5486c1061c93f0b)**  
 by rounddelta on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/07

**[SPJ の憂鬱 (和訳) - Qiita](https://qiita.com/Pythonissam/items/41f8121795f5d8954802)**  
 by rounddelta on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/17

**[SPJとHaskellのエコシステム（和訳） - Qiita](https://qiita.com/reotasosan/items/d9fdfab10e24eabfefda)**  
 by reotasosan on [Haskell (その4) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell4) 12/18

**[Haskell のパフォーマンスをデバッグする](https://haskell.e-bigmoon.com/posts/2017-12-27-haskell-performance-debugging)**  
 by waddlaw on [Haskell (その5) Advent Calendar 2017 - Qiita](http://qiita.com/advent-calendar/2017/haskell5) 12/01
