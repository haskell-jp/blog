---
title: Haskellにおける記号の調べ方
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: YAMAMOTO Yuji
postedBy: <a href="http://the.igreque.info/">YAMAMOTO Yuji(@igrep)</a>
date: December 25, 2021
---

この記事は[ググって解決しづらかったこと Advent Calendar 2021](https://qiita.com/advent-calendar/2021/gseach)の25日目の記事です。[Haskell-jp WikiのHaskellの歩き方](https://wiki.haskell.jp/Hikers Guide to Haskell)というページにもほぼ同じことを書きましたが、今回はよい機会なので実例を加えつつ詳しく紹介させてください。

# 二項演算子（記号関数）の調べ方

よく知られているとおり、Haskellでは二項演算子をプログラマーがかなり自由に定義できるという、とても変わった特徴があります。他のプログラミング言語でも使う標準的なもの（例: `+`, `*`, `&&`など）を名前空間を絞って置き換えるほか、[例えばかのlensパッケージのように](https://hackage.haskell.org/package/lens-5.1/docs/Control-Lens-Operators.html)、ライブラリーの作者があたかも新しい構文を作り上げるかのごとく独自の二項演算子を提供することができます。

これは面白い機能ではあるものの、しばしば混乱を招く機能でもあります。後述するユーザーが定義した演算子でない記号との区別がつきにくいですし、一般的な検索エンジンで検索することさえままなりません。[Googleはプログラミングでよく使われる記号による検索をサポートはしている](https://blog.fkoji.com/2017/03052055.html)ものの、Haskellでしか見ないような記号の組み合わせは到底無理でしょう。

そんな背景もあり、Haskellを使う人はしばしば[Hoogle](https://hoogle.haskell.org/)などの、関数名で検索できる検索エンジンを使用することになります。こちらは二項演算子の名前での検索もサポートしています。

例えばlensパッケージでおなじみの`^.`で検索すると[次のような結果になりました](https://hoogle.haskell.org/?hoogle=%5E.):

![Hoogleによる検索結果の例](/img/2021/symbols-in-ghc/hoogle.png)

lensパッケージ以外でも、同様の`^.`が定義されているのが分かりますね。lensパッケージは依存関係がとても大きい一方、`^.`などの定義は十分単純でコピペしてもいいくらい小さいので、このようにいくつものパッケージで定義されています。

また、特によく使われる二項演算子はFPCompleteのウェブサイトでもまとめられています:

[Operator Glossary](https://www.fpcomplete.com/haskell/tutorial/operators/)

# ユーザーが定義した二項演算子ではないものの調べ方

Haskell、というかそのデファクトスタンダードな処理系であるGHCでは、[言語拡張](https://haskell.jp/blog/posts/2018/about-ghc-exts-1.html)という形で長年新しい構文が提案されています[^ghc-proposals]。その中には、当然これまでにない方法で記号を使っているものもあります。そうした記号はプログラマーが定義した関数ではないので、前述のHoogleなどを使った方法が通用しません。そこで、当ブログにも何度も寄稿いただいた\@takenobu\_hsさんが、言語拡張によるものも含めた、Haskellの構文における記号の一覧を作ってくださいました！

[takenobu-hs/haskell-symbol-search-cheatsheet](https://github.com/takenobu-hs/haskell-symbol-search-cheatsheet)

[^ghc-proposals]: 余談: [ghc-proposals](https://github.com/ghc-proposals/ghc-proposals)に送られたPull requestを見ると、今どのような提案が議論されているか分かります。

実は日本語版も[Qiitaに](https://qiita.com/takenobu-hs/items/b95f0a4409c59440d4a9)あるのですが、上記のGitHub版の方が更新されているようです。そこで、今回はおまけとしてGHCに最近（バージョン9.2.1以降に）追加された、新しいピリオド `.` の使い方を紹介しましょう。

従来、Haskellでピリオドといえば関数合成を表す二項演算子でした:

```haskell
ghci> f x = x + 1
ghci> g x = x * 3
ghci> h = g . f
ghci> h 2
9 -- 2 に + 1 して * 3 した結果
```

数学における関数合成の記号「g ∘ f」に似せてピリオドを採用したのでしょう。しかし、世は今まさに**大「ピリオドといえばフィールド[^field]へのアクセス演算子じゃろがい」時代**です。それでなくてもHaskellのレコード型は扱いにくいと言われているのに、フィールドへのアクセスまで変なやり方でした[^prefer]:

```haskell
data SomeRecord =
  SomeRecord { field1 :: String, field2 :: Int }

someRecord = SomeRecord "value1" 2

ghci> field1 someRecord
"value1"

ghci> field2 someRecord
2
```

[^field]: 他のプログラミング言語では「プロパティー」と呼ばれることも多いですが、ここではHaskellのレコード型における用語に合わせました。

[^prefer]: 個人的にはゲッターが関数になるのはとても直感的な気がして割と好きでしたが、確かにデメリットもとても多い仕様でした。セッターは単純な関数になってないですしね。

そこで、GHC 9.2からは[`OverloadedRecordDot`](https://downloads.haskell.org/ghc/9.2.1/docs/html/users_guide/exts/overloaded_record_dot.html#overloaded-record-dot)という言語拡張が導入され、これを有効にしたファイルではおなじみの言語のようにピリオドでレコードのフィールドにアクセスできるようになりました:

（以下はGHCiで使用した例です）

```haskell
ghci> :set -XOverloadedRecordDot

ghci> someRecord.field1
"value1"

ghci> someRecord.field2
2

-- ⚠️ピリオドの前後に空白を入れると関数合成として解釈されてしまう！
ghci> someRecord . field2

<interactive>:5:1: error:
    ? Couldn't match expected type ‘Int -> c’
                  with actual type ‘SomeRecord’
    ? In the first argument of ‘(.)’, namely ‘someRecord’
      In the expression: someRecord . field2
      In an equation for ‘it’: it = someRecord . field2
    ? Relevant bindings include
        it :: SomeRecord -> c (bound at <interactive>:5:1)
```

`OverloadedRecordDot`についてのより詳しい解説は、[Haskell Day 2021における、fumievalさんの発表](https://youtu.be/haZl-q6mfyk?t=2581)をご覧ください。

# まとめ

- 他のプログラマーが定義した、二項演算子（記号関数）を調べるときは:
    - [Hoogle](https://hoogle.haskell.org/)
    - 補足: Stackageの最新のLTSから検索したいときは[Stackage](https://www.stackage.org/)のページ上部にあるフォームで検索してみましょう。こちらも内部はHoogleが使われています。
- それ以外の場合は:
    - [takenobu-hs/haskell-symbol-search-cheatsheet](https://github.com/takenobu-hs/haskell-symbol-search-cheatsheet)
- Haskellのレコード型に嫌気が差したら:
    - [Haskell Day 2021における、fumievalさんの発表](https://youtu.be/haZl-q6mfyk?t=2581)を観て[`OverloadedRecordDot`拡張](https://downloads.haskell.org/ghc/9.2.1/docs/html/users_guide/exts/overloaded_record_dot.html#overloaded-record-dot)について勉強しましょう。

🎁それでは2022年もHappy Haskell Hacking!!🎅
