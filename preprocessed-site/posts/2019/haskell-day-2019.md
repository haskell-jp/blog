---
title: Haskell Day 2019を開催しました！
subHeading: ""
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: YAMAMOTO Yuji
postedBy: <a href="http://the.igreque.info/">YAMAMOTO Yuji (@igrep)</a>
date: November 17, 2019
tags: イベント
---

先日2019年11月9日、[TECH PLAY SHIBUYA](https://techplay.jp/space)にて[Haskell Day 2019](https://techplay.jp/event/727059)を開催しました。  
今回は、各発表の概要や、アンケートの結果をお伝えしたいと思います。

# 発表

まずは各種発表の紹介から。

## 関数型(function type)を見つめるプログラミング

「関数型」 --- すなわちHaskellでいうところの`a -> b`で表される、Haskellの関数について、ちょっと見方を変えた新しい発見を教えてくれました。

[資料はこちら](https://github.com/nobsun/hday2019/blob/master/doc/ftype.pdf)

<img src="/img/2019/haskell-day-2019/nobsun.jpg" width="808" height="540" />

## HKD(Higher Kinded Datatype)

Higher Kinded Datatype (HKD)という、昨今Haskell界で流行りの型定義方法を解説しています。  
レコード型を定義する際HKDにすることで、より柔軟に扱うことができるようになります。  
さらに、[barbies](http://hackage.haskell.org/package/barbies)や[extensible](http://hackage.haskell.org/package/extensible)といった、HKDの利用を飛躍的に促進するパッケージも紹介されました。

[資料はこちら](https://assets.adobe.com/public/b93f214d-58c2-482f-5528-a939d3e83660)

<img src="/img/2019/haskell-day-2019/fumieval.jpg" width="674" height="450" />

## 「しんさんきぼう」のDerivingストラテジー

Haskellの`deriving`機能 --- 型を定義したとき、型クラスのインスタンスまで自動で定義してくれるあの機能ですね --- の、適用範囲を広げるGHCの言語拡張をいろいろ紹介してくれました。

[資料はこちら](https://aiya000.github.io/Maid/haskell-day-2019-deriving/#/)

<img src="/img/2019/haskell-day-2019/aiya000.jpg" width="729" height="487" />

## HaskellメタプログラミングによるEgisonのパターンマッチの実装

プログラミング言語[Egison](https://www.egison.org/ja/)の核となる機能である強力なパターンマッチを、GHCの各種拡張を駆使することで、Haskellのソースコードに自然に埋め込めるような形で実装した、という話です。

[資料はこちら](https://www.egison.org/download/20191109HaskellDay.pdf)

[紹介しているライブラリーのソースコードはこちら](https://github.com/egison/egison-haskell)

<img src="/img/2019/haskell-day-2019/egison.jpg" width="808" height="540" />

## 関数と型で理解する自動微分

関数の自動微分を行うパッケージ[ad](http://hackage.haskell.org/package/ad)の仕組みを自力で実装してみることで解説してくれました。

[資料はこちら](https://speakerdeck.com/lotz84/guan-shu-toxing-deli-jie-suruzi-dong-wei-fen)

<img src="/img/2019/haskell-day-2019/lotz.jpg" width="808" height="540" />

## GHCJS によるWebフロントエンド開発

[miso](http://hackage.haskell.org/package/miso)というおいしそうな名前のアプリケーションフレームワークと、Firebaseと連携するmisoのサンプルを、ライブコーディングを通して紹介してくれました。  
misoを使えば、GHCJSを使ってElm Architecture風の設計に基づいてアプリケーションを作ったり、さらにそのコードを利用してサーバーサイドレンダリングをしたりできます。

ℹ️資料はまだ公開されていません！当日はライブコーディングが大半の時間を占めていたため、同等の解説を文章にして公開したいというチェシャ猫さんの意向によるものです。  
現在執筆中のためお待ちください。🙇

[発表中に使用したソースコードはこちら](https://github.com/y-taka-23/miso-firebase-tutorial)

<img src="/img/2019/haskell-day-2019/y_taka_23.jpg" width="808" height="540" />

## Haskellで作る競技型イベントの裏側

「mixi git challenge」というイベントにおいてユーザーが投稿した解答を採点するサーバーを、HaskellとElmで一から書き直した、という事例を発表してくれました。  
[rio](http://hackage.haskell.org/package/rio)や[servant](http://hackage.haskell.org/package/servant)といった著名なパッケージを使うだけでなく、足りないところを自力で補って新しいパッケージとして公開したり、さらに作成したアプリケーション自体をOSSとして公開したりすることで、大きな資産を残していただけました。

[資料はこちら](https://www.slideshare.net/noob00/haskell-191796924)

[紹介しているアプリケーションのソースコードはこちら](https://github.com/matsubara0507/git-plantation)

<img src="/img/2019/haskell-day-2019/matsubara0507.jpg" width="808" height="540" />

## 大規模数値計算を支える Haskell ── Pragmatic Haskell in Large-Scale Numerical Computation──

[DeepFlow株式会社](https://www.deepflow.co.jp/)におけるHaskellの事例の紹介です。  
超高速で大規模な数値計算システムを、GHCの多様な言語拡張を駆使して作っているそうです。  
Tagless Finalを活用することで知っているべき領域を区分して仕事を分けることに成功しているという点が印象的でした。

[資料はこちら](https://speakerdeck.com/konn/da-gui-mo-shu-zhi-ji-suan-wozhi-eru-haskell-nil-nil-pragmatic-haskell-in-large-scale-numerical-computation-nil-nil)

<img src="/img/2019/haskell-day-2019/mr_konn.jpg" width="808" height="540" />

## Cadenza: Building fast functional languages on the JVM

[cadenza](https://ekmett.github.io/cadenza/cadenza/index.html)という、Truffle（GraalVMに含まれている、高速なインタープリター作成フレームワーク）製の関数型言語の紹介です。  
Truffleがもたらす強力なJITと「Normalization by Evaluation」という技術を応用することで、型検査と実行時両方における高いスピードを得ることが狙いだそうです。
将来的には依存型言語における型チェックや、GHCのランタイムの高速化に寄与したいとのことです。

[資料はこちら](https://drive.google.com/file/d/1bwYO66xUKeHyR4YCNm_1C82JlDNQLUXv/view)

[紹介しているアプリケーションのソースコードはこちら](https://github.com/ekmett/cadenza)

<img src="/img/2019/haskell-day-2019/ekmett.jpg" width="808" height="540" />

## LT

今回はHakell Day史上初めての試みとして、Lightning Talkを当日公募しました。  
残念ながら5分間という短い制限時間に収められない発表が大半でしたので、ぜひ👇の資料を読んでみてください！

**順番が間違っていたら済みません！ご指摘を！**

- [3D Model in Haskell - Haskellで3Dモデルに触れる](https://docs.google.com/presentation/d/1TiDWz3zLUwEWgpzXfgVZFIib6JtYriB03TVgHsimJC0/edit#slide=id.gc6f73a04f_0_0)
- [HaskellでIoTやってます](https://speakerdeck.com/cyclone_t/iot-cases-with-haskell)
- QuoraでHaskellへの愛を語る（資料なし）
- [Haskellで作ってわかる型クラス](https://gitpitch.com/coord-e/slide-type-class-impl)
- [Abstract Typeclasses - How To Design a Future-Proof Typeclass](https://drive.google.com/file/d/1YGKjl8S-LlfuB8yrHnKSK5G5MGsP9xd3/view)
- [GHCのGC](http://www.mew.org/~kazu/material/2019-gc.pdf)

# アンケート結果

# おわりに

以上の発表のに加えて、今回は、下記のスポンサー企業の皆様や@fumievalくんのおかげで、大変満足度の高い懇親会ができました。

- [マーベリック株式会社](https://www.mvrck.co.jp/)
- [株式会社HERP](https://herp.co.jp/)
- [株式会社インターネットイニシアティブ](https://www.iij.ad.jp/)
- [株式会社クリプタクト](https://www.cryptact.com/)

TODO: 懇親会の写真

発表について。  
昨年は「[Haskellちょっと興味あるからちょっとできるまで](https://haskell-jp.connpass.com/event/92617/)」というテーマを意識して、発表の難易度別に時間帯が分かれるよう調整しましたが、残念ながらうまくいきませんでした。  
そこで難易度調整の難しさを痛感したため、今回は敢えて難易度調整を行わなかったのです。  
結果、全体として難しい発表に偏ってしまった点は少し反省です。[私が会社で開いているHaskell勉強会](https://eng-blog.iij.ad.jp/archives/3467)に毎回参加いただいている同僚も、総じて難しくて追いつくのが大変だった、と仰ってました。  
次回は特別に難易度を下げた発表枠をいくつか作り、内容を事前に精査する、なんてプランを考えています。

会場について。  
TECHPLAY SHIBUYAは素晴らしいですね！  
我々のようなお金がないコミュニティーが今回のような規模のイベントを行うのにうってつけでした。  
元々イベントを開催する前提で作られており、受付と演壇が近いため受付しながら発表を聞くことができるのも、持ち回りで受付をしている我々にとって好都合でした。  
来年も是非使わせていただきたいです🙏

最後に。  
発表者のみなさんはもちろん、支えていただいたスタッフ、スポンサー企業、会場まで足を運んでいただいた参加者の皆様、その他この会の実現に関わったすべての方々に、この場を借りて感謝の意を示したいと思います。  
みなさんのご協力おかげで、大きなトラブルもなく、楽しいイベントが開催できました。ありがとうございます。  
今後も日本Haskellユーザーグループ（a.k.a. Haskell-jp）をよろしくお願いします！  
hask(\_ \_)eller

# あわせて読みたい

- 参加者による参加レポート:
    - [今日は Haskell Day 2019 の日です - 北海道苫小牧市出身の初老PGが書くブログ](http://hiratara.hatenadiary.jp/entry/2019/11/09/110030)
    - [Haskell Day 2019に参加しました - ncaq](https://www.ncaq.net/2019/11/09/20/56/00/)
    - [Haskell Day 2019 に参加してきた](https://matsubara0507.github.io/posts/2019-11-10-join-haskell-day-2019)
- [去年の開催レポート](https://haskell.jp/blog/posts/2018/haskell-day-2018.html)
