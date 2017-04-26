
# [Haskell-jp](https://haskell.jp)ブログ

[English version](./README.md)

[![Build Status](https://secure.travis-ci.org/haskell-jp/blog.svg)](http://travis-ci.org/haskell-jp/blog)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

このプロジェクトは[Haskell-jp](https://haskell.jp)のブログです。
Haskell製の静的サイト生成ツール[Hakyll](http://jaspervdj.be/hakyll/index.html)を使っています。
基本的なビルドなどの操作は、`Makefile`を使えば簡単にできるようになっています。
`Makefile`を使う場合は、事前に`stack`をインストールしておいてください。

### `stack`のインストール

`stack`のインストール手順は[stack README](https://github.com/commercialhaskell/stack#how-to-install)を見てください。
できれば公式サイトを見たほうがいいですが、たぶん日本語情報もたくさんあります。
`stack`さえインストールされて`PATH`に追加されていれば、以降の`Makefile`の各種コマンドが使えるようになっているはずです。

### ブログのビルド

```
$ make site
```

実際のHTMLファイルなどを生成します。
生成されたファイル群は`generated-site/`ディレクトリ下にあります。

### 自動で再ビルドする開発サーバを立ち上げる

```
$ make watch
```

ブログのコンテンツを自分用に見れるようにする開発用サーバを立ち上げることができます。
コンテンツに変更があった場合には、自動的にビルドしなおして、リアルタイムに変更をチェックできます。

### 生成されたファイルをすべて削除する

```
$ make clean
```

`generated-site/`や`.hakyll-cache/`などの生成されたファイルを全て削除します。
また、`stack clean`もこのコマンド内で実行します。
`stack clean`.

### ブログのデプロイ

```
$ make deploy
```

最初にサイトをビルドし、その生成されたファイル群を`gh-pages`ブランチにコミットします。
ちょっと無理のあるやり方ですが、だいたいこれでうまくいっています。

### 下書きとして公開する

`posts/2017-03-25-sample-post.markdown`がサンプルの記事です。
`draft: true`というオプションをつけることで、`/drafts`下に、
URLを知っている人のみがアクセスできる状態で記事を公開できます。
これは、主に誰か忙しい人にレビューを依頼する際に用います。
通常は`draft: true`オプションをつけずに、プルリクエストとしてレビューしてもらってください。
