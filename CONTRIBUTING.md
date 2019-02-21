# 記事を寄稿する方法

Haskell-jp Blogの寄稿に興味を持っていただいてありがとうございます！  
記事を寄稿していただける場合、下記の流れで行います。

1. 投稿したい人が、[preprocessed-site/posts/](https://github.com/haskell-jp/blog/tree/master/preprocessed-site/posts/)というディレクトリーに、Markdownで記事を書いて置いてください。
    - GitHubのアカウントをお持ちであれば、上記のリンク先にある、"Create new file"というボタンから追加できるはずです。
    - 記事の先頭に書く内容などは、同じディレクトリーにある、適当なほかの記事を参考にしてください。
    - 内部でPandocを使用しているので、[Pandocがサポートしている構文](http://pandoc.org/MANUAL.html#pandocs-markdown)であれば、すべて利用できます。
1. 作成した記事を含めたコミットで、Pull requestを送ってください。先ほどの"Create new file"というボタンからの導線に従えば、割と簡単にできるはずです。
1. [GitHubのHaskell-jp organization](https://github.com/haskell-jp)に所属する人などが、記事をレビューします。適宜対応してください。
1. 送ったPull requestがマージされると、CIが自動で記事を公開してくれます！

# 記事のライセンスについて

原則として、次のルールが適用されます。

- 寄稿者が執筆した記事の著作権は、**寄稿者のもの**となります。
- その上で、寄稿者が執筆した記事に対しては、**「[クリエイティブ・コモンズ 表示 4.0 国際 ライセンス](https://creativecommons.org/licenses/by/4.0/)（通称CC-BY 4.0）」**が適用されます。
    - 従って、Haskell-jp Blogに公開される記事は、寄稿者以外の人が、寄稿者の名前を表示させた上で、自由に再配布したり、改変したりすることができるという点を、あらかじめご了承ください。
    - 詳細は[クリエイティブ・コモンズ 表示 4.0 国際 ライセンスの条文](https://creativecommons.org/licenses/by/4.0/legalcode.ja)をご覧ください。
- ただし、寄稿者以外の人がGitHubのPull requestやIssue報告などを通じて寄稿者の記事を修正する場合、著作権は、**記事の著作者の同意の下、記事の寄稿者に委譲**するものとします。Pull requestを送った人や、Issueを報告した人のものとはなりません。

もし記事のライセンスについて、何かしら特別な事情がある場合、GitHubのIssueを通じてご相談ください。例外的な対応も、適宜検討します。

# 記事にして欲しい内容を提案する方法と注意点

記事の寄稿ではなく、記事にして欲しい内容を提案していただける場合は、[このリンク](https://github.com/haskell-jp/blog/issues/new?template=topic-request.md&labels=Topic+Request)より Issue を作成してください。
Issue には、どのような記事を書いてほしいか書いてください。
例えば:

- ○○パッケージの使い方やサンプルが知りたい
- 数学用語と Haskell 用語の対応関係が知りたい
- 少し古めの Haskell 本を読む上での注意点が知りたい
- などなど

**ただし、知見の持ち主が居ないかもしれませんし、誰かの負担になるものなので必ず記事になるとは限りません。**
また、場合によっては既に記事があるため、既存の記事を薦められるかもしれません。
