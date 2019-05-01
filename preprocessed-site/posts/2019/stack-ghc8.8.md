---
title: GHC 8.8.1 alphaをstackでダウンロードして手持ちのパッケージをビルドする
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: May 2, 2019
tags: GHC, stack
...
---

先日、我らがHaskellのデファクトスタンダードなコンパイラー、[GHCのバージョン8.8.1-alpha1がリリースされました](https://mail.haskell.org/pipermail/ghc-devs/2019-April/017550.html)。  
このリリースはまだアルファ版であることからわかるとおり、主にテスト目的で使用するためのものです。  
なのでいち早く試してみて、GHCのデバッグに貢献してみましょう。

そこで今回は、最近Haskellを始めた方なら使っている方も多いであろう、stackを使ってこの新しいGHCをインストールし、あなたのライブラリー・アプリケーションでテストする方法を紹介いたします。

# TL;DR cabal-installでやったほうがよさそう

いきなりやろうとすることを真っ向から否定するようで恐縮ですが...😅  
実際に私が試しにビルドしてみた感じ、普通に[cabal-installをこちらから](https://www.haskell.org/cabal/download.html)インストールして、`cabal new-build --with-ghc=ghc-8.8.0.20190424`などと実行した方がいいんじゃないかという気がしました...。  
cabal-installにはGHCをインストールする機能はないので、その場合はGHCは別途インストールすることになります<small>（[`ghcup`](https://github.com/haskell/ghcup)が使える？）</small>。  
[\@takenobu-hsさんが書いてくれた、こちらの記事](../2017/06-ghc-install.html)を参考にどうぞ！

なお、stackでやると面倒な理由についての詳細はこれから述べる手順で適宜触れます...。

# 1. `setup-info`を作る

まずはじめに、stackがGHCをインストールする際に参照する、[`setup-info`](https://docs.haskellstack.org/en/stable/yaml_configuration/#setup-info)というYAMLを作りましょう。  
`setup-info`は`stack setup`や`stack build`を実行したとき、GHCなどの必要なソフトウェアがインストールされていなかった際、自動でGHCをインストールするために必要な情報です。  
GHCのバージョンや対象となるプラットフォームごとに、GHCのビルド済みtarballへのURLやそのチェックサムが書いてあります。  
stackはここに書かれたURLにアクセスすることで、GHCをインストールしているんですね。

デフォルトでは、stackは[こちらのYAML](https://raw.githubusercontent.com/commercialhaskell/stackage-content/master/stack/stack-setup-2.yaml)ファイルを`setup-info`として扱っています。  
このYAMLには[Stackage](https://www.stackage.org/)が参照している、安定版のGHCについては書いてあるものの、LTS HaskellにもStackage Nightlyにもまだ採用されていないGHCについては、書かれていません。  
当然アルファ版であるGHC 8.8.1-alpha1が書かれることはないため、GHC 8.8.1-alpha1用の`setup-info`を作る必要があります。

それでは書いてみましょう... と、言いたいところですが、この`setup-info`、実際のところ自分で直接書く必要はなく、YAMLファイルへのURLやパスを指定するだけでstackは参照しに行ってくれます！  
と、言うわけで、[こちらにGHC 8.8.1-alpha1向けの`setup-info`](https://gist.github.com/igrep/7298e1e2515059ae332feaf5501c41a4)を作ってアップロードしておきました！  
<small>（申し訳なくもLinuxについてはどう書けばいいかわからず、macOSとWindows 64bitのみ対応いたしました... あしからず。🙇）</small>

ひとまずみなさんは、下記のいずれかの方法で指定するだけでこの手順はクリアできます。

- `stack.yaml`に記載する:  
  ```yaml
  resolver: ghc-8.8
  setup-info: "https://gist.githubusercontent.com/igrep/7298e1e2515059ae332feaf5501c41a4/raw/d69cc0b75d9be6735bdfcca6aa3eb6398d98983f/stack-setup-info.yaml"
  # ... 以下略 ...
  ```

    - ビルドしたいプロジェクトや、GHC 8.8を試す用のディレクトリーを作って、そこに👆の内容が書かれた`stack.yaml`を置きましょう。  
      ちょっと試したいだけならそのディレクトリーで`stack exec ghci`などと実行すればOKです！
- `stack setup`コマンドのオプションとして渡す:  
  ```
  stack setup 8.8.0.20190424 --setup-info-yaml https://gist.github.com/igrep/7298e1e2515059ae332feaf5501c41a4/raw/d69cc0b75d9be6735bdfcca6aa3eb6398d98983f/stack-setup-info.yaml
  ```

    - `--setup-info-yaml`オプションを指定した上で`8.8.0.20190424`という引数を与えるのがポイントです。  
      GHCの開発版の慣習上、`8.8.1-alpha1`**ではなく**`8.8.0.20190424`となっている点に注意してください！

「8.8.1-alpha1じゃなくて、自分でビルドしたGHCを`stack`でインストールできるようにしたい！」というマニアなあなたは、[今回私が作った`setup-info`](https://gist.github.com/igrep/7298e1e2515059ae332feaf5501c41a4)をどうぞ参考にしてください！🙇

# 2. （必要なら）allow-newerを有効にする

ここからは、何かしら依存するパッケージがあるライブラリー・アプリケーションをGHC 8.8.1-alpha1で試しにビルドしたいという方向けです。  
GHC 8.8.1-alpha1をちょっと試したいだけという方はこれ以降を読む必要はありません。

まずは、ひとまず対象となるプロジェクトの`stack.yaml`に

```yaml
allow-newer: true
```

を追記しましょう。  
これは、依存しているCabalパッケージのバージョンの、上限を取っ払うというものです。  
依存パッケージのバージョンの上限は、パッケージの開発者が自身のパッケージを確実にビルドできるよう、「このパッケージはあのパッケージのバージョンN.M**以下**じゃないとビルドできないよ！」とCabalの依存関係リゾルバーに教えてあげるためのものです。  
cabal-installやstackは、通常であればこの上限を見て、どのバージョンのパッケージをインストールするか決めます[^stack]。  
その上限により、残念ながら依存関係の解決に失敗することがあるのです。  
そこでそうしたエラーを避けるためにも`allow-newer: true`と設定して、上限を無視してみましょう。

[^stack]: stackは、通常Stackage（に、登録されたresolver）に書いてあるバージョンのパッケージを使用しますが、Stackageに登録されていないパッケージも必要であることが判明した場合、この「バージョンの上限」を利用して、追加のパッケージのバージョンを決める（はず）です。

というのも、このバージョンの上限はしばしば、予防のために実際より厳しめに設定されることがあるためです[^upper]。  
そりゃそうですよね。今作っているパッケージが依存しているAPIが、どのバージョンで使用できなくなるかなんて大抵のパッケージではわかりませんし。  
Haskellの世界には[PVP](https://pvp.haskell.org/)という、[Semantic Versioning](https://semver.org/lang/ja/)と似た思想のバージョン変更ポリシーがありまして、APIの互換性がなくなるような修正が含まれる場合、次のバージョンでは`A.B.C`の`A.B`の箇所を変更することになっています。  
これを信じて依存バージョンの上限（と下限）を設定してみても、実際にあなたが依存しているAPIが使用できなくなるとは限らないのです。

[^upper]: もっとも、私のようにものぐさな人間が作るパッケージには、そもそも上限も何も書いてないことが多いのですが...😰

したがって、依存パッケージのバージョンの上限は、実際には無視してもよい場合がしばしばあります。  
もちろん、自分で依存パッケージのバージョンを正しく書き換えて対応するというのもアリですし、将来的にはそうした方がより望ましいやり方です。  
しかし、今回は手っ取り早くビルドしてみるために、敢えて`allow-newer: true`を設定することと致しました。  
「私はバージョンの上限を直してみたいんだー！」という方は、是非チャレンジしてみてください。

# 3. package-indicesを設定して、head.hackageを利用できるようにする

`stack.yaml`に書いておいた方が良い設定がもう一つあります。  
それは、[HEAD.hackage](http://head.hackage.haskell.org/)の設定です。

これからビルドするあなたのパッケージは、きっとたくさんのパッケージに依存していることでしょう。  
残念ながら、そのうちGHC 8.8に対応できていないものも数多くあるでしょう😰。  
特に今回は[`MonadFail` Proposal](https://scrapbox.io/haskell-shoen/MonadFail)による、`Monad`型クラスの仕様変更を適切に周知できていなかったこともあり、まだ多くのパッケージが対応できていないようです。

しかし、まだ希望はあります。  
あなたの依存パッケージに対する必要な修正は、すでにmasterブランチにマージされているかも知れませんし、すでに誰かがPull requestを送っているかも知れません。  
さらにラッキーな場合、HEAD.hackageにパッチを当てたバージョンが上がっていることでしょう！

HEAD.hackageは、今回のようにGHCの開発版をいち早く試したい人が、新しいGHCに向けて修正を加えたパッケージを、いち早くアップロードするサイトです。  
[こちらのリポジトリー](https://github.com/hvr/head.hackage)にパッチをアップロードすることで、cabal-installやstackから、普通のhackageにあるパッケージとしてダウンロードできるようにしてくれます。

HEAD.hackageをstackで利用するには、下記のように、`package-indices:`という設定を、`stack.yaml`に加えてください。  
下記のように記載することで、stackは、HEAD.hackageにある修正済みのパッケージを優先して取得してくれるようになります[^security]。

[^security]: 本来であればHackage Securityの設定も必要なはずなんですが、なぜかうまくいかず...😱。[こちら](https://github.com/commercialhaskell/stack/issues/3844)で紹介されたworkaroundにしたがって、関連する設定を除くことにしました...。

```yaml
package-indices:
  - name: head.hackage
    download-prefix: http://head.hackage.haskell.org/package/
    http: http://head.hackage.haskell.org/01-index.tar.gz
  - name: Hackage
    download-prefix: https://hackage.haskell.org/package/
    http: https://hackage.haskell.org/01-index.tar.gz
```

これでGHC 8.8対応済みのパッケージを、簡単に取得できるようになります！

# 4. stack buildを実行しつつ、ひたすらextra-depsを追加・編集

ここまで設定できたら、いよいよ`stack build`してみましょう！  
とは言え、この状態では間違いなく失敗が続くので、`stack build --file-watch`と、**`--file-watch`オプションを着けて、`stack.yaml`を編集する度に再度**ビルドが実行されるようにするのをおすすめします。

と、言うのも、恐らく次👇のようなエラーがたくさん出ると思われるからです。

```
...
In the dependencies for wss-client-0.2.1.1:
    http-client must match >=0.5.13, but the stack configuration has no specified version  (latest
                matching version is 0.6.4)
    http-client-tls needed, but the stack configuration has no specified version  (latest matching
                    version is 0.3.5.3)
    network-uri needed, but the stack configuration has no specified version  (latest matching
                version is 2.6.1.0)
    websockets must match >=0.12.0 && <0.13, but the stack configuration has no specified version
               (latest matching version is 0.12.5.3)
needed since wss-client is a build target.

Some different approaches to resolving this:

  * Consider trying 'stack solver', which uses the cabal-install solver to attempt to find some
    working build configuration. This can be convenient when dealing with many complicated
    constraint errors, but results may be unpredictable.

  * Recommended action: try adding the following to your extra-deps
    in C:\Users\igrep\Downloads\direct-hs\stack-ghc-8.8.yaml:

attoparsec-0.13.2.2@sha256:6a0baba19991e84ef939056e7b411ad3a1ea0fb5e1e8fce7ca50e96c84b206c8
base-compat-0.10.5@sha256:d49e174ed0daecd059c52d13d4f4de87b5609c81212a22adbb92431f9cd58fff
...
```

このエラー、見かけたことがある人も多いでしょう。  
そう、指定したresolver<small>（stackが使用するパッケージのバージョンの一覧。Stackageに登録されている`lts-13.12`などもその一つ）</small>に、必要なバージョンのパッケージが登録されていない場合に起こるエラーです。  
みなさんが普段利用する`lts-13.12`などのresolverでは、数多くのパッケージが登録されています<small>（[最新版のLTS Haskell 13.19](https://www.stackage.org/lts-13.19)で2346件。Stackageをメンテしている皆さんのおかげですね）</small>。

一方、最初の手順で我々が指定したresolver、すなわち`resolver: ghc-8.8`は、GHC 8.8に添付されたパッケージ<small>（`base`パッケージや、`array`パッケージなど）</small>しか入っていない、実質空っぽなresolverなのです<small>（[参考](https://docs.haskellstack.org/en/stable/yaml_configuration/#resolver)）</small>。  
そのため、あなたが必要なほとんどのパッケージはないため、stackはやむなく「`extra-deps`にこれらのパッケージを追加してね！」というエラーを出すことになります。  
これではstackの良さを生かせません...。cabal-installで`cabal new-build`していれば、cabal-installは黙って必要なパッケージのバージョンを決定し、あとは`cabal new-freeze`でもすれば、完全にビルドを再現可能な状態にしてくれます。  
やっぱりstackはあくまでもStackageを活かすためのツールと捉えた方がいいのかも知れません😥。

`extra-deps`へのパッケージの記載を何度か繰り返すと、ようやくパッケージのビルドが始まります。  
HEAD.hackageに収録されたパッケージを正しく取得できていれば、現在Hackageにアップロードされているバージョンではビルドできない依存パッケージも、無事ビルドできることでしょう。  
依存するパッケージの数にもよりますが、やっぱり時間がかかるかと思います。待ちましょう☕️。

## それでもうまくいかない場合: `extra-deps`を使い倒す

しかしやっぱり、必要な変更が施されたパッケージが、HEAD.hackageにもアップロードされていない場合はあります。  
そうした場合、自分で修正して<small>（Pull requestを送りつつ）</small>パッチを[HEAD.hackageのリポジトリー](https://github.com/hvr/head.hackage)にアップロードすることもできますが、`stack.yaml`の`extra-deps`を次のように使えば、もっと手っ取り早く修正したバージョンのビルドを試すことができます。

### 自分以外の人が対象のパッケージを修正した場合:

自分以外の人が対象のパッケージを修正したので、すでにどこかのリポジトリーにpush済みのコミットがある、という場合、下記👇のように書くと、Gitリポジトリーの特定のコミットを直接参照した状態で、依存関係に加えることができます。

```yaml
extra-deps:
- git: https://github.com/github_user/repository_name.git
  commit: <修正したコミットのSHA>
```

### 自分で対象のパッケージを修正する、という場合:

そうでない場合、対象のパッケージのリポジトリーを一旦`git submodule add`して、自分のリポジトリーの一部に含めてしまいましょう。  
その上で、`extra-deps`には下記のように書けば、stackはローカルのファイルシステムに置かれたディレクトリーも、直接依存するパッケージとして追加してくれます。

```yaml
extra-deps:
- ./path/to/package
```

逐一別のディレクトリーに`git clone`して`git commit`して`git push`して作られたコミットのSHAを参照して... なんてのを繰り返していたら、面倒だからです。

### 対象のパッケージがGitリポジトリーで管理されてない場合は？

臨機応変に対応しましょう...😰  
ちなみに、[extra-depsのドキュメント](https://docs.haskellstack.org/en/stable/yaml_configuration/#git-and-mercurial-repos)いわくstackはMercurialもサポートしています。

# 番外編: Operation Vanguard

以上がstackを使ったGHC 8.8-alpha1のインストール方法や、それを利用したパッケージのビルド手順です。自分でGHCをビルドしたときなども参考にしてみてください。  
これで終わり...！と、言いたいところですが、GHC 8.8に関連して、非常に意欲的なプロジェクト💪を紹介させてください。

それは、[Operation Vanguard](https://github.com/haskell-vanguard/haskell-vanguard)です。  
[\@fumieval](https://github.com/fumieval/)さんが始めた、「エコシステムの主要なパッケージの最新版を一挙にGHC 8.8に対応させる」プロジェクトです。  
一旦submoduleとして対象のパッケージのリポジトリーをcloneする、という方法は、Operation Vanguardのリポジトリーを見ていて知りました💡。

すでに対応のほとんどが終了したとのことですが、GHC 8.8に対応していないパッケージは恐らくまだたくさんあります。  
ゴールデンウィークももう半分が終わりましたが、時間をとってOperation Vanguardのようにチャレンジしてみるのはいかがでしょうか💪💪💪
