# [Haskell-jp Blog](https://haskell.jp/blog)

[![Build Status](https://secure.travis-ci.org/haskell-jp/blog.svg)](http://travis-ci.org/haskell-jp/blog)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

このリポジトリーでは[Haskell-jp Blog](https://haskell.jp/blog)の記事や、記事の内容などについての問題を管理しています。  
広くHaskellに関する記事を常に募集しています！  
寄稿方法は下記をご覧ください。

# 記事の投稿方法

Haskell-jp Blogの寄稿に興味を持っていただいてありがとうございます！  
寄稿される場合は下記の手順で行ってください。  
基本的には投稿する人は、**Markdownで記事を書いて、Pull requestを送るだけ**です！

1. 投稿したい人が、[preprocessed-site/posts/](https://github.com/haskell-jp/blog/tree/master/preprocessed-site/posts/)というディレクトリーに、Markdownで記事を書いて置いてください。
    - GitHubのアカウントをお持ちであれば、上記のリンク先にある、"Create new file"というボタンから追加できるはずです。
    - 記事の先頭に書く内容などは、同じディレクトリーにある、適当なほかの記事を参考にしてください。
    - 内部でPandocを使用しているので、[Pandocがサポートしている構文](http://pandoc.org/MANUAL.html#pandocs-markdown)であれば、すべて利用できます。
1. 作成した記事を含めたコミットで、Pull requestを送ってください。先ほどの"Create new file"というボタンからの導線に従えば、割と簡単にできるはずです。
1. [GitHubのHaskell-jp organization](https://github.com/haskell-jp)に所属する人などが、記事をレビューします。適宜対応してください。
1. 送ったPull requestがマージされると、CIが自動で記事を公開してくれます！
    - [諸般の事情](https://github.com/haskell-jp/blog/issues/54)により、このときのビルドは現状Travis CIが実行します。

# 記事の内容や、ウェブサイトの構成などについての問題があった場合

その他、記事の内容や、ウェブサイトの構成などについての問題があった場合、[GitHubのissue](https://github.com/haskell-jp/blog/issues/new)でご連絡ください。
