---
title: 相互リンク集
headingBackgroundImage: ../img/background.png
headingDivClass: post-heading
subHeading: Haskell-jpが応援するWebサイト一覧
author: Haskell-jp
date: May 18, 2017
...
---

[このほかに、Haskell-jp Wikiにもリンク集があります！](https://wiki.haskell.jp/Links)

# Haskell-jpが応援するWebサイト一覧

この一覧への[登録方法は下記](#howToGetSupport)をご覧ください。

<ul>
<li id="ccvanishing.hateblo.jp" class="hash-target"><a href="http://ccvanishing.hateblo.jp/">チェシャ猫の消滅定理</a> by チェシャ猫</li>
<li id="fumieval.hatenablog.com" class="hash-target"><a href="http://fumieval.hatenablog.com">モナドとわたしとコモナド</a> by Fumiaki Kinoshita</li>
<li id="hiratara.hatenadiary.jp" class="hash-target"><a href="http://hiratara.hatenadiary.jp/">北海道苫小牧市出身のPGが書くブログ</a> by hiratara</li>
<li id="itchyny.hatenablog.com" class="hash-target"><a href="http://itchyny.hatenablog.com/">プログラムモグモグ</a> by itchyny</li>
<li id="kakkun61.hatenablog.com/archive/category/Haskell" class="hash-target"><a href="http://kakkun61.hatenablog.com/archive/category/Haskell">趣味はデバッグ……</a> by 岡本和樹</li>
<li id="khibino.github.io/haskell-relational-record" class="hash-target"><a href="http://khibino.github.io/haskell-relational-record/">Haskell Relational Record project page</a> by khibino, yuga, kazu-yamamoto</li>
<li id="kurokawh.blogspot.jp" class="hash-target"><a href="http://kurokawh.blogspot.jp/">Kuro's Blog</a> by Hiroyuki Kurokawa (hrkr)</li>
<li id="lotz84.github.io" class="hash-target"><a href="https://lotz84.github.io">flip map</a> by lotz</li>
<li id="matsubara0507.github.io" class="hash-target"><a href="https://matsubara0507.github.io/">ひげメモ</a> by matsubara0507</li>
<li id="syocy.hatenablog.com" class="hash-target"><a href="http://syocy.hatenablog.com/">syocy’s diary</a></li>
<li id="the.igreque.info" class="hash-target"><a href="http://the.igreque.info">igreque : Info</a> by Yuji Yamamoto</li>
<li id="www.sampou.org" class="hash-target"><a href="http://www.sampou.org">{算法|算譜}.ORG</a> by Nobuo Yamashita (@nobsun)</li>
</ul>

# この一覧への登録方法 {#howToGetSupport}

登録条件: 広い意味でプログラミング言語Haskellに関連したことが書いてあるWebサイトであること。

1. [こちらのリポジトリーでIssueを作成](https://github.com/haskell-jp/blog/issues/new?title=相互リンク作成依頼：（あなたのWebサイトの名前）&body=（一行目にあなたのWebサイトのURLを書いてください。）%0D%0A%0D%0A運営者の氏名（ハンドルネーム可）: （必須ではありません）%0D%0A%0D%0A（何かあればここに一言コメントをこちらに。）)してください。
1. 担当者が承認すると同時に、必要なHTMLのスニペットを提示するので、それをあなたのサイトに張り付けてください！
    - バナーの画像はSVGなので、大きさは適当にいじっていただいてかまいません！ただ、あまりにも小さくしたり、アスペクト比を変えるのはやめていただきたいですが...。
1. 以上！

## 承認する担当者へ

TODO: 承認を自動化するツールを作る。

1. 対象のWebサイトを承認する場合は、承認する旨を伝えるとともに、下記のスニペットをコピペして、`(siteId)`と書かれた箇所を「追加するWebサイトのURLからスキーム(`http://`や`https://`の部分)を取り除いたもの」で置き換え、対象のWebサイトに貼り付けてもらうようIssueのコメントで依頼しましょう。
    ```html
        ```html
        <a href="https://haskell.jp/blog/posts/links.html#(siteId)"><img width="234" src="https://haskell.jp/img/supported-by-haskell-jp.svg" alt="Supported By Haskell-jp."></a>
        ```
    ```
1. 続いて、下記のmarkdownのスニペットにおける`(siteId)`の部分を、先ほどの「追加するWebサイトのURLからスキームを取り除いたもの」で置き換え、[このページを編集](https://github.com/haskell-jp/blog/edit/master/preprocessed-site/posts/links.md)し、追加しましょう。

    ```html
    <li id="(siteId)" class="hash-target"><a href="(WebサイトのURL)">(追加するWebサイトの名前)</a>( by 運営者の氏名があれば)</li>
    ```
