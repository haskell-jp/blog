---
title: Haskell-jp Blogへの投稿が簡単になりました！
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
subHeading: Advent Calendarの記事の投稿も募集しております！
author: Haskell-jp
tags: Haskell-jp, CircleCI, Travis CI
date: November 27, 2017
...
---

こんにちは。Haskell-jpです。  
ちょっと間が空いてしまいましたが、久々の投稿です。Haskell自体の話と関係なくてすみません！

内容としてはタイトルのとおりなのですが、[こちら](https://github.com/haskell-jp/blog/pull/51)や[こちら](https://github.com/haskell-jp/blog/pull/53)をはじめとするPull requestにより、当ブログの記事の投稿が簡単になりました！  
具体的には、下記の点を改善しております。

- 記事を作成してPull requestを送った際、\[Merge pull request\]ボタンを押してmasterブランチにマージしただけで自動で記事が公開されるようになりました！
- [CircleCIのartifacts機能](https://circleci.com/docs/1.0/build-artifacts/)を利用することにより、ビルド結果から実際に公開される際のページをプレビューできるようにしました！
    - *https://XX-XXXXXXXX-gh.circle-artifacts.com/0/home/ubuntu/blog/generated-site* みたいなURLで見られるようになります（具体的なページはビルド結果ごとに異なるので、適宜ご案内します）。

結果、これまでHaskell-jp Blogに投稿する際に問題となっていた、下記の点が解消されました。

- 投稿する人が自分で`make`などを実行しなければ、markdownで書いた記事がどのようなHTMLに変換されるかわからなかった。
- 権限を持った人が`make deploy`するまで、記事をmasterブランチにマージしても公開されなかった。

以上を踏まえた、Haskell-jp Blogの投稿手順については、[README](https://github.com/haskell-jp/blog#readme)をご覧ください。  
基本的に投稿する人は、**Markdownで記事を書いて、Pull requestを送るだけ**です！

それでは、これからもHaskell-jp Blogをよろしくお願いします！ hask(\_ \_)eller  
なお、現在Haskell-jp Blogでは、[Haskell Advent Calendar 2017](https://qiita.com/advent-calendar/2017/haskell)（と[その2](https://qiita.com/advent-calendar/2017/haskell2)、[その3](https://qiita.com/advent-calendar/2017/haskell3)）の記事を特に精力的に募集しています。  
ぜひこの機会にHaskell-jp Blogに記事を投稿してみませんか？
