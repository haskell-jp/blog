---
title: HIW 2019で発表された、GHC 8.10に導入されるであろう機能
subHeading: ～HIW 2019参加レポート その2～
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: September 16, 2019
tags: GHC, Haskell Implementors' Workshop
...
---

[前回](/posts/2019/hiw-ghc8.8.html)から引き続き、[Haskell Implementors' Workshop 2019](https://icfp19.sigplan.org/home/hiw-2019#About)への参加レポートとして、私の印象に残った発表をいくつか紹介します。  
今回は、「GHC 8.10に導入されるであろう機能」です。  
いずれも該当するMerge Requestはmasterブランチにマージ済みなので、おそらくGHC 8.10で提供されるでしょう。

## HoleFitPlugins and the future of interactive development in GHC

- 発表者: Matthías Páll Gissurarson *Chalmers University of Technology, Sweden*
- 該当のMerge Request: [!153](https://gitlab.haskell.org/ghc/ghc/merge_requests/153)
- 該当のGHC Proposal: なし

[昨年のHaskell Symposiumでも発表](https://icfp18.sigplan.org/details/haskellsymp-2018-papers/10/Suggesting-Valid-Hole-Fits-for-Typed-Holes-Experience-Report-)されてGHC 8.6で導入された、「Valid Hole Fits」という機能のさらなる拡張について。

まず、「Valid Hole Fits」という機能について軽く紹介します<small>（詳しくは[こちらのスライド](https://wataru86.github.io/slides/vhs/)が参考になるかと思います）</small>。  
「Valid Hole Fits」はアンダースコア `_`で始まる識別子を書いたとき、GHCが推論した型にマッチする関数をエラーメッセージに付記することで、ユーザーがどんな式を書けばよいか、ヒントを与えてくれるものです。

例えば、

```haskell
map (length . _someFunc) [True, False, True]
```

上記のように、アンダースコア `_`で始まる識別子を書いたとき、

```haskell
<interactive>:1:16: error:
    ...
      Valid hole fits include
        enumFrom :: forall a. Enum a => a -> [a]
          with enumFrom @Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Enum’))
        show :: forall a. Show a => a -> String
          with show @Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Show’))
        repeat :: forall a. a -> [a]
          with repeat @Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.List’))
        return :: forall (m :: * -> *) a. Monad m => a -> m a
          with return @[] @Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Base’))
        pure :: forall (f :: * -> *) a. Applicative f => a -> f a
          with pure @[] @Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Base’))
        mempty :: forall a. Monoid a => a
          with mempty @(Bool -> [a0])
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Base’))
```

といった具合に、アンダースコアで始まる識別子`_someFunc`の型を`Bool -> [a0]`と推論した上で[^type-hole]、実際にその型に該当する関数を、当該のスコープにおいてアクセスできる関数の中から探して教えてくれる、それが「Valid Hole Fits」という機能です。

[^type-hole]: 復習: この、「アンダースコアで始まる識別子、`_someFunc`の型を`Bool -> [a0]`と推論した上で」エラーメッセージにおいて`Found hole: _someFunc :: Bool -> [a0]`と教えてくれるのが「Type Hole」という機能なのでした。

今回発表された「HoleFitPlugins」という機能は、名前のとおりこの「Valid Hole Fits」に対するプラグイン機構です。  
「Valid Hole Fits」が表示する「型にマッチした関数」を探す処理を、Haskellのコードで書き換えられるようにしてくれます！

「そこまでする必要あるの？」という気もしてきますが、発表者曰く

- Hoogleをはじめ、TensorFlowなどGHCの外部にあるものを利用して「型にマッチした関数」を探せるようにするために必要
- GHCiとこの機構を組み合わせることで、もっとインタラクティブな開発を促進したい

という意図があるそうです。

最新安定版のGHCでは利用できませんが、[ドキュメントがこちら](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/extending_ghc.html#hole-fit-plugins)にあるので、GHCのHEAD<small>（masterブランチで開発中のバージョン）</small>をコンパイルすれば使用できるようです。

加えて発表では、`_`で始まる識別子を書く際の構文を拡張することで、どのようにcandidateを探すか指定できるようにする、なんて機能も紹介されました<small>（ドキュメントを読む限りこの機能はまだHEADに入ってない？）</small>。  
例えば、Hoogleを使ってValid Hole Fitsを探したいとき、次のように書くことで検索対象を`Control.Applicative`に限定する、といったことをできるようしてくれます。

```hskell
g :: [a] -> [[a]]
g = _{hoogleLookup "+Control.Applicative"}
```

Valid Hole Fitsの検索方法をその場で微調整したい、というときに使うものですね。

## Visible dependent quantification

- 発表者: Ryan Scott *Indiana University at Bloomington, USA*
- 該当のMerge Request: [!378](https://gitlab.haskell.org/ghc/ghc/merge_requests/378)
- 該当のGHC Proposal: [0081-forall-arrow](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0081-forall-arrow.rst)

タイトルのとおり、「Visible dependent quantification」という機能の紹介です。

最近のバージョンのGHCiにおける`:kind`コマンドは、次のような、GHCがサポートしていない構文の型注釈を出力することがあります。  
例えば

```haskell
> :set -XKindSignatures
> :set -XPolyKinds
> data SomeType k (a :: k)
> :kind SomeType
SomeType :: forall k -> k -> *
```

における、`SomeType :: forall k -> k -> *`の`forall k ->`という部分です。  
現在のHaskellで`forall k`などと書くときは、必ず

```haskell
SomeType :: forall k. k -> *
```

といった具合に、ピリオドで区切った構文になります。  
ところが先ほどの`:kind`の出力では、`forall k ->`とあるとおり、`forall k`に<small>（型ではなく、カインドとしての）</small>関数を表す`->`が使われています。  
「Visible dependent quantification」はまさにこれを、`:kind`コマンドによって出力される構文だけではなく、ユーザーが直接書ける構文にしよう、というものです。
GHCに「依存型」という機能を加える「Dependent Haskell」にも必要な機能だそうです。  
私自身はこの機能を使う機会がちょっと思い浮かばなかったので省略しますが、より詳しい解説は発表者である[Ryan自身による記事（英語）](https://ryanglscott.github.io/2019/03/15/visible-dependent-quantification-in-haskell/)をご覧ください。何が「Visible」でどう「Dependent」なのかわかるはずです。
