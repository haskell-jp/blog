---
title: Haskell Antenna をリニューアルしました
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: MATSUBARA Nobutada
postedBy: <a href="https://matsubara0507.github.io/whoami">MATSUBARA Nobutada(@matsubara0507)</a>
date: March 21, 2018
tags: Haskell-jp
---

[Haskell Antenna](https://haskell.jp/antenna/)は[lotz84](https://github.com/lotz84)氏が作ったHaskellの日本語情報を収集するウェブサイトです。
下記の記事を読むと、動機付けなどが分かると思います。

- [Haskell Antenna を公開しました - Haskell-jp](https://haskell.jp/blog/posts/2017/03-haskell-antenna.html)

残念なことにHaskell Antennaは動作が重く、なかなか満足に閲覧することが出来ませんでした。
そこで、Haskell Antennaをリニューアルしました！

正確には、[Planet Haskell](https://planet.haskell.org/)の日本語版として作成した[もの](https://github.com/matsubara0507/planet-haskell-jp-demo)を、新しいHaskell Antennaとして置き換えました。
新Antennaは旧Antennaと比べると見た目も機能も更新頻度も残念なことになってしまいましたが、各サイトのフィードから記事の一覧を取得し静的サイトとして生成しているだけなので動作は軽快です。

旧Antenna同様に新Antennaでも配信する情報源(今のところAtomかRSS2.0形式のフィード)をいつでも募集しています。
もし追加すべき情報源にアイデアがあれば[GitHubレポジトリのREADME](https://github.com/haskell-jp/antenna#サイトの追加方法)にかかれている方法を参考にPull Requestを送っていただくことが可能です。
また、PRを送るのは面倒だという方はHaskell-jpのSlackの#antennaチャンネルを通じて提案を行ってもらうことも大歓迎です。
(Planet Haskellがそうであるように)Haskell中心でなくても良いので、Haskellの情報を発信しているブログを持っている方は是非、追加提案をしていただけると助かります。
