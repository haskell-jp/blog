---
title: Haskell-jp Blogへの投稿が簡単になりました！
headingBackgroundImage: ../../img/post-bg.jpg
headingDivClass: post-heading
subHeading: Advent Calendarの記事の投稿も募集しております！
postedBy: Haskell-jp
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

以上を踏まえた、Haskell-jp Blogの投稿手順は下記のようになります。

1. 投稿したい人が、[preprocessed-site/posts/](https://github.com/haskell-jp/blog/tree/master/preprocessed-site/posts/)というディレクトリーに、Markdownで記事を書いて置いてください。
    - GitHubのアカウントをお持ちであれば、上記のリンク先にある、"Create new file"というボタンから追加できるはずです。
    - 記事の先頭に書く内容などは、同じディレクトリーにある、適当なほかの記事を参考にしてください。
    - 内部でPandocを使用しているので、[Pandocがサポートしている構文](http://pandoc.org/MANUAL.html#pandocs-markdown)であれば、すべて利用できます。
1. 作成した記事を含めたコミットで、Pull requestを送ってください。先ほどの"Create new file"というボタンからの導線に従えば、割と簡単にできるはずです。
1. [GitHubのHaskell-jp organization](https://github.com/haskell-jp)に所属する人などが、記事をレビューします。適宜対応してください。
1. 送ったPull requestがマージされると、CIが自動で記事を公開してくれます！
    - [諸般の事情](https://github.com/haskell-jp/blog/issues/54)により、このときのビルドはTravis CIが実行します。

基本的に投稿する人は、**Markdownで記事を書いて、Pull requestを送るだけ**です！

それでは、[Haskell Advent Calendar 2017](https://qiita.com/advent-calendar/2017/haskell)（と[その2](https://qiita.com/advent-calendar/2017/haskell2)、[その3](https://qiita.com/advent-calendar/2017/haskell3)）の記事を始め、これからもHaskell-jp Blogをよろしくお願いします！ hask(\_ \_)eller
