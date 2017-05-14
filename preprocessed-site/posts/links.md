---
title: 相互リンク集
headingBackgroundImage: ../img/post-bg.jpg
headingDivClass: post-heading
subHeading: Haskell-jpが応援する個人のWebサイト一覧
postedBy: Haskell-jp
date: May 28, 2017
...
---

[このほかにも、Haskell-jp Wikiにもリンク集があります！](https://wiki.haskell.jp/Links)

# Haskell-jpが応援する個人のWebサイト一覧

この一覧への[登録方法は下記](#howToGetSupport)をご覧ください。

<ul>
<li id="the.igreque.info" class="hash-target"><a href="http://the.igreque.info">igreque : Info</a> by Yuji Yamamoto</li>
</ul>

# この一覧への登録方法 {#howToGetSupport}

登録条件: 広くプログラミング言語Haskellに関連したことが書いてあるWebサイトであること。

1. [こちらのリポジトリーでIssueを作成](https://github.com/haskell-jp/blog/issues/new?title=相互リンク作成依頼：（一行目にあなたのWebサイトの名前）&body=（一行目にあなたのWebサイトのURLを書いてください。）%0D%0A%0D%0A運営者の氏名（ハンドルネーム可）: （必須ではありません）%0D%0A%0D%0A（何かあればここに一言コメントをこちらに。）)してください。
1. 担当者が承認すると同時に、必要なHTMLのスニペットを提示するので、それをあなたのサイトに張り付けてください！
    - バナーの画像はSVGなので、大きさは適当にいじっていただいてかまいません！ただ、あまりにも小さくしたり、アスペクト比を変えるのはやめていただきたいですが...。
1. 以上！

## 承認する担当者へ

TODO: 承認を自動化するツールを作る。

1. 対象のWebサイトを承認する場合は、`(siteId)`と書かれた箇所を「追加するWebサイトのURLからスキーム(`http://`や`https://`の部分)を取り除いたもの」で置き換え、作成されたIssueにコメントしましょう。
    ```html
    <a href="https://haskell.jp/blog/posts/links.html#(siteId)"><img width="234" src="https://haskell.jp/img/supported-by-haskell-jp.svg" alt="Supported By Haskell-jp."></a>
    ```
1. 続いて、下記のmarkdownのスニペットにおける`(siteId)`の部分を、先ほどの「追加するWebサイトのURLからスキームを取り除いたもの」で置き換え、[このページを編集](https://github.com/haskell-jp/blog/edit/master/preprocessed-site/posts/links.md)し、追加しましょう。

    ```html
    <li id="(siteId)" class="hash-target"><a href="(WebサイトのURL)">(追加するWebサイトの名前)</a>( by 運営者の氏名があれば)</li>
    ```