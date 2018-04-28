---
title: 素のGHCをローカルディレクトリにインストールする方法
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: takenbu.hs
date: August 8, 2017
...
---

# はじめに

素(単独)のGHCのバイナリをローカルディレクトリにインストールする方法について簡単に紹介します。  
GHCの新リリースが出た場合などに、stack や haskell-platform 経由ではなく、手軽にインストールして遊べます。
以下、Linux系での手順です。 ユーザー権限で行えます。


# インストール方法

## 1. リリース物をdownloadします

GHC単独品の配布ディレクトリ（ https://www.haskell.org/ghc/ ）から、目的のバージョンのGHCを選びます。  

例えば、Linux(Debian, Ubuntu, ...)用の ghc 8.2.1 のバイナリであれば、以下から、`ghc-8.2.1-x86_64-deb8-linux.tar.xz` を選びます。

  https://www.haskell.org/ghc/download_ghc_8_2_1.html#linux_x86_64


## 2. tarファイルを展開します

```sh
$ tar -xvJf ghc-8.2.1-x86_64-deb8-linux.tar.xz
```

## 3. インストール先のローカルディレクトリを作ります

```sh
$ mkdir ghc821
```

## 4. configします

tarを展開したトップディレクトリにcdで移動します。
さらに、`--prefix`オプションで、GHCのインストール先のディレクトリを指定します。

```sh
$ cd ghc-8.2.1
$ ./configure --prefix=/home/my/ghc821
```

## 5. installします

tarを展開したトップディレクトリで、makeコマンドを実行します。
```sh
$ make install
```

これで、`--prefix` オプションで指定したディレクトリの `bin/`下に、`ghc`, `ghci`, `runghc` などがインストールされます。  
なお、インストール後は、tarで展開した側のディレクトリ以下は削除して構いません。

## 6. 遊ぶ

`bin/`ディレクトリ下の、`ghc`, `ghci`, `runghc` などで遊べます。 例えば、以下のようにしてghciを起動できます。

```
$ ghc821/bin/ghci
GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help
Prelude> 1+2
3
```

ghc8.2.1のカラフルなエラーメッセージや、compact-regionなど、新機能を手軽に楽しめます、enjoy! :)

もちろん、環境変数PATHにインストール先のディレクトリを追加しておけば、デフォルトでこのGHCを使用できます。


# 補足

## 補足1

本格的にGHCでprojectを作る場合には、素のGHCでなく、stack や haskell-platform をインストールするのが良いでしょう。

 * [stack](https://haskell-lang.org/get-started)
 * [haskell-platform](https://www.haskell.org/platform/)


## 補足2

環境によっては、libgmpが無いというエラーが発生する場合があります。  
その場合は、以下の対処方法があります。
```
# apt-get install libgmp10
# cd /usr/lib/x86_64-linux-gnu
# ln -s libgmp.so.10 libgmp.so
```

