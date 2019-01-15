---
title: TidalCyclesをstackで確実にインストールする
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: January 17, 2019
tags: Windows, TidalCycles
...
---


# 背景

先日teratailのHaskellタグを眺めていたところ、下記のような質問がありました。

[Haskell - networkパッケージがうまく機能しない｜teratail](https://teratail.com/questions/167461?rss)

[TidalCycles](https://tidalcycles.org/index.php/Welcome)という、Haskell製の内部DSLでシンセサイザーの演奏ができるライブラリーのインストールがうまくいかない、という質問です。  
networkパッケージがインストールできていない、ということなのでcabal hellにでもハマったのかな、と思ったのですが、[類似しているとおぼしき報告](https://github.com/tidalcycles/tidal-chocolatey/issues/1)（と、[こちら](https://qiita.com/yoppa/items/fe21d7136f8f3aafd55c#comment-b568fc7ecb423b9bc2ce)）を読む限り、どうもGHCのインストール自体に何か問題があるように見えました。

もう当の質問者はHaskell Platformをインストールすることで解決したそうですが、いずれにしても、我々Haskellerとしては、stackなりcabal new-installなりといった、慣れた方法でインストールできた方がサポートしやすいですし、きっと確実です。  
というわけで今回はstackでのインストールに挑戦してみました。  
すでにstackをインストールしているというHaskell開発者は多いでしょうし、そうした方がTidalCyclesを使いたくなったときの参考になれば幸いです。

結論から言うとほとんど問題なくできたんですが、以下のtweetで述べたポイントにご注意ください。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">TidalCycles, Atomのpackageの設定でstack exec ghciを使うよう設定したら普通にstackで入れたGHCから使えましたわ。<br>ポイントは、<br>- hosc-0.17のstack.yamlのextra-depsに加えないといけない<br>- WindowsでGHC 8.6.3は地雷なのでLTS 12.26を使う<br>- ~/.ghciで:set +mしてるとうまく動かない<br>ぐらいか。</p>&mdash; Yuji Yamamoto: 山本悠滋 (@igrep) <a href="https://twitter.com/igrep/status/1082475580753207296?ref_src=twsrc%5Etfw">2019年1月8日</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>


# 実行した環境

- Windows 10 Pro 64bit ver. 1809
- `stack --version`: Version 1.9.1, Git revision f9d0042c141660e1d38f797e1d426be4a99b2a3c (6168 commits) x86\_64 hpack-0.31.0
- TidalCycles: [1.0.6](http://hackage.haskell.org/package/tidal-1.0.6)
- SuperCollider: 3.10.0, 64bit
- Atom: 1.34.0
- Atomのtidalcyclesプラグイン: 0.14.0

# 各種依存パッケージのインストール

冒頭に上げた質問をされた方が参考にしたページ [TidalCyclesのインストール2018年版 - Qiita](https://qiita.com/yoppa/items/fe21d7136f8f3aafd55c) では、Chocolateyを使ったインストール方法を紹介していますが、この方法では、直接GHCのWindows向けtarballをダウンロードしてインストールしているようです。  
私が知る限り特にその方法でも問題はないはずなんですが、なぜか質問者が挙げたようなエラーが発生してしまいます。  
また、TidalCyclesが実行時に依存しているSuperColliderやSuperDirtといったソフトウェアを、別のChocolateyのパッケージに分けることなく、TidalCyclesのインストールスクリプトで直接インストールしているようです<small>（詳細は[Chocolateyのパッケージ情報](https://chocolatey.org/packages/TidalCycles)に書かれたchocolateyinstall.ps1を参照されたし）</small>。  
そのため、ChocolateyでTidalCyclesをインストールしようとすると、問題のあるGHCと、SuperColliderなどの依存パッケージを一緒にインストールしなければなりませんし、SuperColliderやSuperDirtだけをChocolateyでインストールすることもできません。

なので、ここは素直に[TidalCycles公式のWiki](https://tidalcycles.org/index.php/Windows_installation)に書かれた方法に従ってSuperColliderやSuperDirtをインストールしつつ、Haskell関連のものだけstackでインストールしようと思います。

## [TidalCycles公式のWiki](https://tidalcycles.org/index.php/Windows_installation)そのままの手順

**⚠️行く先々でWindowsのファイアウォールの警告が出るかと思います。適当に承認しちゃってください！⚠️**

1. [SuperColliderを公式サイトからインストール](https://supercollider.github.io/download)します。  
  今回は「Windows」の箇所に書いてある「3.10.0, 64-bit (no SuperNova)」というリンクをクリックしてダウンロードされた実行ファイルをインストールしました。
1. [Atom](https://atom.io/)も公式サイトからインストールしました。  
  後で触れますTidalCyclesの対話環境を、Atom上で呼び出すためのプラグインがあるためです。他のエディタ向けのプラグインもありますが、公式サイトで紹介していたのはAtomなので、一番これがサポートされているのでしょう。
1. GitもPrerequisitesとして上げられていますが、すでに私の環境に入っているので今回は特に何もしていません。なければ普通に[Git for Windows](https://gitforwindows.org/)を入れるのが無難かと思います。
1. SuperDirtのインストール
    1. SuperColliderをスタートメニューから起動します。
    1. ウィンドウの左側にある「Untitled」と書かれた箇所の下がSuperColliderのエディタになっているようです<small>（色がわかりづらい！）</small>。  
      そこに`include("SuperDirt")`と書いて、「Shift + Enter」を押せば、SuperDirtのインストールが始まります。
    1. 次のセクションでSuperDirtを起動する前に、**一旦SuperColliderを終了**させましょう。
1. Atom向けtidalcyclesプラグインのインストール
    - 面倒なので省略します。他のプラグインと変わらないはずなので適当に検索してください！

## TidalCycles公式のWikiとは異なる手順

ここからはこの記事特有の手順です。  
最近のHaskell開発者は、[stack](https://docs.haskellstack.org/en/stable/README/)というツールを使って開発環境を整えることが多いですので、冒頭の予告通りここではstackを使います。  
ちなみに、現在はHaskell Platformにもstackが添付されていますが、Haskell Platformに含まれる、GHCはstackを使うことでも簡単にインストールできるため、stackのみをインストールすれば十分です。  
なお、stack自体のインストール方法については拙作の[「失敗しながら学ぶHaskell入門」のREADME](https://github.com/haskell-jp/makeMistakesToLearnHaskell#%E3%81%BE%E3%81%A0stack%E3%82%84haskell-platform%E3%82%92%E3%82%A4%E3%83%B3%E3%82%B9%E3%83%88%E3%83%BC%E3%83%AB%E3%81%97%E3%81%A6%E3%81%84%E3%81%AA%E3%81%84%E5%A0%B4%E5%90%88%E3%81%AF)をご覧ください。  
Windowsではインストーラーをダウンロードして実行するだけで十分でしょう。

stackのインストールが終わったら、次の手順を踏んでください。

### stackでのTidalCyclesのビルド

stackでTidalCyclesのビルドをするには、`C:\sr\global-project\stack.yaml`というファイルを、下記でコメントしたように書き換えてください。

```yaml
# ... 省略 ...
packages: []
resolver: lts-12.26 # <= ここを編集

extra-deps:         # <= この行と、
- hosc-0.17         # <= この行を追記
```

簡単に編集した内容について解説させてください。

まず、`resolver:`で始まる行ですが、これは「LTS Haskell」という、パッケージの集合のバージョンを指定するものです。  
「LTS Haskell」は、「確実にビルドできるバージョンのパッケージをまとめた一覧」です。  
LTS Haskellのメンテナーの方々は、毎日登録された大量のパッケージをまとめてビルド・テストしてみることで、実際に登録されたバージョンのパッケージのビルドとテストが成功することを確認しています。  
なので、このLTS Haskellに登録されているバージョンのパッケージを使う限りは、私たちは安心してビルドができると言うことです。

なぜLTS Haskellのバージョンを書き換えたのかというと、それは、LTS Haskellには実際にはパッケージの集合だけでなく、それらをビルドできるGHCのバージョンも含まれているからです。  
したがって、LTS Haskellのバージョンを指定する、ということは、そのままインストールするGHCのバージョンも指定することになります[lts-haskell]。  
実は特に今回の場合、インストールするGHCのバージョンを指定しなければ、ビルドできない可能性が高かったのです。  
現在の最新のLTS Haskellに登録されているGHCのバージョンは「8.6.3」ですが、残念ながらこのバージョンのGHCには、[Windows版のみにおいて深刻なバグ](https://ghc.haskell.org/trac/ghc/ticket/16057)があります。  
実際にTidalCyclesをビルドする際にこのバグに遭遇するかは確かめてませんが、内容からして遭遇する確率が高そうであるという点と、遭遇するとビルドができないという点を考慮して、念のため確実にビルドできるバージョンのGHCを指定しておきました。

[lts-haskell]: どのバージョンのLTS HaskellでどのバージョンのGHCがインストールされるかは、LTS Haskellを管理している[「Stackage」というウェブサイトのトップページ](https://www.stackage.org/)にある、「Latest LTS per GHC version」というセクションをご覧ください。

そして、`extra-deps`という項目は、ビルドしようとしているパッケージ<small>（今回の場合`tidal`パッケージ）</small>が依存しているパッケージが、LTS Haskellに登録されていない場合に指定するものです。  
[tidalパッケージ ver. 1.0.6のパッケージ情報](http://hackage.haskell.org/package/tidal-1.0.6)を確認すると、確かにhoscというパッケージに依存していると書かれていますね！   
残念ながらこのhoscパッケージは今回指定した、LTS Haskellのver. 12.26には登録されていないので、明記しておいてください。

`C:\sr\global-project\stack.yaml`の編集が終わったら、

```bash
stack build tidal
```

と実行しましょう。  
初回はGHCのインストールも含めて行われるので、結構時間がかかると思います。

ちなみに、`stack install tidal`と実行してもいいですが、stackの仕様上、特に結果は変わりません。  
`stack install`は、実行ファイルがついたパッケージをビルドして`PATH`にインストールするためのコマンドなので、`tidal`のように実行ファイルがないパッケージでは意味がありません。

### Atomのプラグインの設定

続いて、Atomのtidalcyclesプラグインの設定をしましょう。  
stackは使用するGHCを、前述のstack.yamlに書いたLTS Haskellのバージョンに応じて切り替える関係上、`PATH`の通ったところにGHCをインストールしません。  
そのため、AtomのtidalcyclesプラグインにstackがインストールしたGHCを認識させるには、下記のように設定を書き換える必要があります。

1. Atomを起動し、「File」 -\> 「Settings」の順にメニューをクリックして、Atomの設定画面を開きます。
1. 画面左側の「📦Packages」と書かれた箇所をクリックすると、インストールしたAtomのプラグインの一覧が表示されるはずです。
1. 一覧から「tidalcycles」を探して、「⚙️Settings」をクリックします。
1. 「Ghci Path」という設定項目があるので、それを`stack exec ghci`に書き換えてください。

# 使い方・動作確認

## TidalCyclesを起動する度に必要になる手順

※[公式サイトのこちらのページ](https://tidalcycles.org/index.php/Start_tidalcycles_and_superdirt_for_the_first_time)に対応しています。

1. SuperDirtの起動
    1. SuperColliderをスタートメニューから起動します。
    1. 先ほど`include("SuperDirt")`と入力した、SuperColliderのエディタに、今度は`SuperDirt.start`と入力して、同じく「Shift + Enter」しましょう。  
      SuperDirtが起動します。
1. Atom上でのTidalCyclesの起動
    1. Atomを起動して、拡張子が`.tidal`なファイルを開くか作成します。
    1. メニューを「Packages」 -\> 「TidalCycles」 -\> 「Boot TidalCycles」の順に選択してください。
    1. 画面下部でGHCiが起動し、TidalCyclesの式を実行するのに必要なパッケージの`import`や、`import`では賄いきれない関数の定義などが自動的に行われます。
        - [BootTidal.hs](https://github.com/tidalcycles/Tidal/blob/master/BootTidal.hs)というファイルを`:load`しているみたいです。
1. 動作確認のために、適当なTidalCyclesの式 --- 例えば公式サイトのWikiどおり`d1 $ sound "bd sn"` --- を入力して、入力した行にカーソルを置き、「Shift + Enter」を押しましょう。
    1. 入力した式が画面下部で起動したGHCiに送信され、実行されます。うまくいっていれば音が鳴るはずです。
    1. 停止させたいときは、`d1 silence`と入力して同じく「Shift + Enter」を押してください。
1. より詳しいTidalCyclesの使い方は、[TidalCyclesのチュートリアル1 - Qiita](https://qiita.com/mk668a/items/6e8e0151817f484a526c)など、他の方が書いた記事を検索してみてください。

# ハマったこと

## 「SuperDirtが見つからない！」という趣旨のエラーが出た

正確なエラーメッセージは申し訳なくも忘れてしまったのですが、SuperCollider上で`SuperDirt.start`と入力した際、エラーになることがあります。  
この場合、SuperColliderを再起動するのを忘れている可能性がありますので、再起動してみてください。  
SuperDirtのインストールを終えた直後では、まだSuperDirtは利用できないのです。

## Atom上でTidalCyclesを起動した際、`parse error`

先ほどの「Atom上でのTidalCyclesの起動」という手順で、`parse error (possibly incorrect indentation or mismatched brackets)`というエラーに出遭うことがあります。  
そのままTidalCyclesの式を入力して「Shift + Enter」しても、`Variable not in scope: d1 :: ControlPattern -> t`などというエラーになってしまうでしょう。  
これは、前のセクションで触れたBootTidal.hsというファイルをGHCiが読み込む際に、エラーになってしまったからです。

原因はいろいろあり得るかと思いますが、私の場合、`~/.ghci`というGHCiの設定ファイルに`:set +m`という行を加えていたためでした。  
まず、`~/.ghci`は、GHCiが起動するときに必ず読み込まれるファイルです。  
必ず有効にしたい言語拡張や、`:set +m`のようなGHCiの設定を記載しておくファイルとなっています。  
要するに`~/.vimrc`などと似たようなものですね。
そして`:set +m`は、GHCiで複数行の入力を有効にするためのものです。  
GHCi上で`:set +m`と実行すると、GHCiは入力した行を見て「あっ、この入力はまだ続きがありそうだな」と判断したとき、次の行を自動で前の行の続きとして扱うようになります。  
そして、その場合入力の終了をGHCiに伝えたい場合は、空行を入力しなければなりません。  
結果、BootTidal.hsを読み込む際に、空行が入力されないため、意図しない行が「前の行の続き」とGHCiに認識されてしまい、`parse error (possibly incorrect indentation or mismatched brackets)`となってしまうようです。

仕方ないので、直すために`~/.ghci`を開いて`:set +m`と書いた行をコメントアウトするか削除しちゃいましょう。  
再びAtomで「Packages」 -\> 「TidalCycles」 -\> 「Boot TidalCycles」の順にメニューをクリックすれば、今度は該当のエラーがなく起動するかと思います😌。

このエラーは、特にすでにHaskellの開発環境を導入している方で遭遇するケースが多いかと思います。ご注意ください。

## SuperDirtを起動し忘れていても何もエラーが起きない

表題の通りです。  
困ったことにSuperDirtを起動し忘れた状態で`d1 $ sound "bd sn"`などの式を実行しても、特に何のエラーもなく、音も鳴りません。  
<small>（サーバーとして起動しているべき）</small>SuperDirtに接続し損ねたんだから、何かしらエラーが表示されてもいいはずなんですが、困ったことにウンともスンとも言いません😰。  
と、言うわけで、何のエラーもなく音も出なかった場合は、SuperDirtを起動し忘れてないか確認しましょう。

# おわりに: Haskell開発者として見たTidalCycles

ここまで、stackという、昨今のHaskellerの多くが好んで利用するツールで、TidalCyclesを利用する方法を説明しました。  
TidalCyclesの公式サイトのWikiにはこの方法は書かれてませんが、より確実なインストール方法として、覚えておいていただけると幸いです。  
すでにHaskellの開発環境をインストールしている方にも参考になるかと思います。

ところで、ここまでTidalCyclesを自分でインストールしてみて、Haskellerとしていくつか気になった点があります。  
TidalCyclesは、Haskell製の内部DSLとしては、ちょっと変わっているように感じました。

それは、TidalCyclesが「標準」として提供している関数を実行する際、tidalパッケージに含まれるモジュールを`import`するだけでなく、BootTidal.hsというファイルを読んで、追加の関数を定義する必要がある、という点です。  
大抵のHaskell製の内部DSLは、そんなことしなくてもモジュールを`import`するだけで使えるようになっています<small>（[Hspec](http://hspec.github.io/)とか[lucid](https://github.com/chrisdone/lucid)とか[clay](http://hackage.haskell.org/package/clay)とか[relational-record](http://khibino.github.io/haskell-relational-record/)とか）</small>。  
つまり本来ならばわざわざ、BootTidal.hsのような、GHCiが読み込む専用のファイルを用意しなくとも良いはずなのです。  
このBootTidal.hsはAtomのプラグインの設定で簡単に切り替えることができるものなので、もし間違ったファイルに設定してしまったら、言語の標準にあたる関数がおかしな動作をすることになりかねませんし、あまり良いやり方だとは思えません。本来なら設定に混ぜて書くべきものではないでしょう。  

なぜTidalCyclesはこんな仕様になっているかというと、それにはある意味Haskellらしい制約が絡んでいると推測されます。  
Atom上でTidalCyclesを起動する、というのは、実際にはGHCiを起動して、[BootTidal.hs](https://github.com/tidalcycles/Tidal/blob/master/BootTidal.hs)というファイルを読み込ませる、ということなのでした<small>（事実、Atomなどのエディターを介さなくとも、お使いのターミナルエミュレーターから`ghci`コマンドを起動してBootTidal.hsファイルを読むだけで、TidalCyclesは利用できます）</small>。  
そのBootTidal.hsの中身を見てみると、サンプルで実行した`d1`という関数が、下記のように定義されていることがわかります。

```haskell
-- ...
import Sound.Tidal.Context

-- total latency = oLatency + cFrameTimespan
tidal <- startTidal (superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/20})

let p = streamReplace tidal

-- ...

let d1 = p 1
let d2 = p 2
let d3 = p 3
-- ...
```

`tidal <- startTidal`で始まる行で、TidalCyclesの初期化を行っていると思われます。  
初期化の際には、サーバーとして起動しているSuperDirtへの接続設定<small>（この場合`127.0.0.1`の`57120`番ポートへ接続している）</small>を渡しているようです。  
恐らくこの`startTidal`関数が、SuperDirtへ接続し、代入した`tidal`という変数に、SuperDirtへの接続を含んでいるんでしょう。  
そして、`let p = streamReplace tidal`という行で、その`tidal`を`streamReplace`関数に[部分適用](http://capm-network.com/?tag=Haskell-%E9%83%A8%E5%88%86%E9%81%A9%E7%94%A8)することで、その`p`に整数<small>（シンセサイザーの番号だそうです）</small>をさらに部分適用した`d1`、`d2`などの関数へ、間接的に`tidal`を渡しています。  
つまり`d1`や`d2`などの関数は、何らかの形で、SuperDirtへの接続情報について知っている必要があるのです。  
DSLとして、`d1`や`d2`などの関数に毎回接続情報を渡すのは煩雑だと考えたのでしょう。  
そして、通常のHaskellがそうであるように、外部のサーバーに接続した結果取得されるものを暗黙に使えるようにしたい場合、 --- つまり、今回のようにユーザーが接続情報を明示的に渡すことなく使えるようにしたい場合 --- 少なくともパッケージを`import`するだけではうまくいきません[^TemplateHaskell]。  
BootTidal.hsのように、SuperDirtのような外部に接続する処理を、GHCiの実行時に書かなければならないのです。

[TemplateHaskell]: 後で軽く触れる、Template Haskellという邪悪なテクニックを使わない限りは。

しかし、`tidal <- startTidal`の行で作られるSuperDirtへの接続情報を`d1`などの関数が暗黙に利用できるようにすることは、実際にはBootTidal.hsで行っているような方法を使わなくともできます。  
そうすることで、BootTidal.hsを変なファイルに切り替えてしまって、`d1`などの関数の定義が間違ったものになってしまう<small>（あるいはそもそも定義されなくなってしまう）</small>リスクを回避できます。  
具体的には、下記のような方法が考えられます。  
申し訳なくも私はこれ以上TidalCyclesに入れ込むつもりもないので、誰かTidalCyclesを気に入った方が適当に提案するなりパッチを送るなりしてみてください<small>（他力本願😰）</small>。

- GHCiの中で`ReaderT`を使う
    - Haskellで「関数に渡した引数を暗黙に利用できるようにする」といえば、やはり`ReaderT`モナドトランスフォーマーが一番オーソドックスな方法でしょう。  
      実はGHCi上では、`IO`以外のモナドのアクションで`print`することができます。  
      [You can override the monad that GHCi uses](https://www.reddit.com/r/haskell/comments/87otrn/you_can_override_the_monad_that_ghci_uses/)というRedditのスレッドでは、`ReaderT`を使ったサンプルが紹介されています。  
      これと同じ要領で、GHCiの`-interactive-print`というオプションに、`tidal`を`ReaderT`経由で渡してから結果を`print`する関数を設定しましょう。  
      あとは`d1`などを`ReaderT`のアクションにするだけで、それらをBootTidal.hsから消し去ることができます。  
      残念ながらこの方法を使うと、GHCiに与えた式の結果がすべて当該のモナドのアクションになってなければならなくなるため、例えば単純な計算結果でも`return`をいちいち書かないと行けなくなります。しかし、TidalCyclesの利用方法を見る限り、大きな問題にはならないだろうと思います。
- `ImplicitParams`というGHCの言語拡張を使う
    - GHCには、`ImplicitParams`という、もっと直接的にこれを実現する言語拡張があります。文字通り、暗黙の引数を実現するための拡張です<small>（[参考](https://qiita.com/philopon/items/e6d2522f5b514c219a5f)）</small>。  
      これを利用して、例えば`d1`を`?tidal :: Stream => ControlPattern -> IO ()`のように型宣言しておき、`?tidal`<small>（頭に`?`を付けたものが暗黙の引数となります）</small>を暗黙の引数として参照するようにしましょう。後はGHCiの起動時に`?tidal`を定義すれば、`?tidal`の後に`d1`などを定義する必要がなくなるので、BootTidal.hsはもっとコンパクトに済むはずです。
- その他、`unsafePerformIO`やTemplate Haskellなど、ちょっと危ない手段を使う
    - こちらについては詳細を割愛します。`d1`などの再利用性が下がるので、おすすめしません。

TidalCyclesの技術的な側面で気になった点は以上です。  
ちょっと難しい話になってしまいましたが、これを機会に、Haskellそのものへの興味を持っていただけると幸いです。  
素晴らしいことに、TidalCyclesそのものはHaskellを知らなくてもそれなりに使えるようになっているようですが、Haskellを知った上で使えば、より簡単にトラブルシューティングができるようになりますし、TidalCyclesをより柔軟に使えるようになるでしょう。

もし、今回の記事やTidalCyclesをきっかけにHaskellを勉強してみたいと思ったら、[Haskell-jp Wikiの日本語のリンク集](https://wiki.haskell.jp/Links)を読んで自分に合う入門コンテンツを見つけてみてください！  
それから、何か困ったことがあれば[Haskell-jpのSlack Workspaceにある、#questionsチャンネル](https://haskell-jp.slack.com/messages/C5666B6BB/convo/C4M4TT8JJ-1547294914.091800/)で質問してみてください。  
[登録はこちら](https://join.slack.com/t/haskell-jp/shared_invite/enQtNDY4Njc1MTA5MDQxLTAzZGNkZDlkMWYxZDRlODI3NmNlNTQ1ZDc3MjQxNzg3OTg4YzUzNmUyNmU5YWVkMjFmMjFjYzk1OTE3Yzg4ZTM)からどうぞ！

それでは2019年もHaskellとTidalCyclesでHappy Hacking!! 🎶🎶🎶
