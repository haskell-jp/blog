---
title: HspecでQuickCheckするときもshouldBeなどが使えます
subHeading: quickcheck-ioパッケージのおかげ
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: February 26, 2020
tags:
...
---

タイトルがほとんどすべてなんですが詳細を解説します。

# 📣`shouldBe`などは`property`の中でも使えるので使ってください！

みなさんはHspecでQuickcheckを使ったProperty testを書く際、どのように書いているでしょうか？  
例えばHspecのマニュアル https://hspec.github.io/quickcheck.html のように、Hspecにproperty testを組み込む例として、次のような例を挙げています。

```haskell
describe "read" $ do
  it "is inverse to show" $ property $
    \x -> (read . show) x == (x :: Int)
```

※[こちらのコミット](https://github.com/hspec/hspec/blob/9f3f4c38952f526701a67b6e26336a3a5aec0e89/doc/quickcheck.md)の時点での話です。

`property`関数に渡した関数<small>（以下、「`porperty`ブロック」と呼びます）</small>の中ではHspecでおなじみの`shouldBe`などのexpectation用関数を使わず、`==`で結果を判定してますよね。  
このサンプルに倣って、Hspecで書いたテストにProperty testを書くときは、`==`を使ってる方が多いんじゃないでしょうか？

ところが、この記事のタイトルに書いたとおり、実際のところ`property`ブロックの中でも`shouldBe`は利用できます。  
つまりは、こちら👇のようにも書ける、ということです！

```haskell
describe "read" $ do
  it "is inverse to show" $ property $
    \x -> (read . show) x `shouldBe` (x :: Int)
```

このように`property`ブロックの中でも`shouldBe`や`shouldSatisfy`といった、Hspec固有のexpectation関数を使うことの利点は、単に構文を他のテストと一貫させることができる、だけではありません。  
**テストが失敗したときのエラーが分かりやすくなる**、という遥かに重大なメリットがあるのです。

試しにわざとテストを失敗させてみましょう。  
先ほどの例:

```haskell
describe "read" $ do
  it "is inverse to show" $ property $
    \x -> (read . show) x == (x :: Int)
```

における`(x :: Int)`という式を`(x + 1 :: Int)`に変えれば、必ず失敗するはずです。

```haskell
describe "read" $ do
  it "is inverse to show" $ property $
    \x -> (read . show) x == (x + 1 :: Int)
```

※お手元で試す場合は[こちら](https://github.com/hspec/hspec/blob/9f3f4c38952f526701a67b6e26336a3a5aec0e89/doc/_includes/QuickCheck.hs)から元のコードを持ってきて、`stack build hspec`なりを実行した上で修正・実行するのが簡単でしょう。

結果、下記のようなエラーメッセージとなるでしょう。

```
...
  1) read, when used with ints, is inverse to show
       Falsifiable (after 1 test):
         0
```

このエラーでは「テストが失敗したこと」と「どんな入力をQuickCheckが生成したか」までしか教えてくれず、わかりづらいですよね。

一方、`shouldBe`を使用して以下のように書き換えると...

```haskell
describe "read" $ do
  it "is inverse to show" $ property $
    \x -> (read . show) x `shouldBe` (x + 1 :: Int)
```

エラーメッセージはこう👇なります。

```
  1) read, when used with ints, is inverse to show
       Falsifiable (after 1 test):
         0
       expected: 1
        but got: 0
```

「テストが失敗したこと」と「どんな入力をQuickCheckが生成したか」に加えて、`shouldBe`に与えた両辺の式がどのような値を返したか、まで教えてくれました！  
今回の例は極めて単純なのであまり役に立たないかも知れませんが、あなたが書いた関数をテストするときはやっぱり「期待される結果」と「実際の結果」両方がわかる方がデバッグしやすいですよね！

と、いうわけで今後は`property`関数<small>（あるいはその省略版の`prop`関数）</small>に渡した関数の中でも`shouldBe`などを必ず使ってください！  
<small>（せっかくなんで、今回紹介したドキュメントを[修正するためのPull request](https://github.com/hspec/hspec/pull/429)を送っておきました。これがマージされればこの記事の情報の大半は時代遅れになります）</small>

# 😕なぜ使える？

しかしここで、一つ疑問が残ります。  
QuickCheckやHspecのドキュメントをつぶさに読んだことがある方はお気づきでしょう。  
QuickCheckの[`property`関数は、`Testable`という型クラスのメソッド](http://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck.html#t:Testable)であるため、`Testable`のインスタンスでなければ使えないはずです。  
Hspecの`shouldBe`などが返す値は型シノニムのたらい回しをたどればわかるとおり、結局のところ`IO ()`型の値です。  
ところが`Testable`のインスタンス一覧を見る限り、`IO a`は`Testable`のインスタンスではありません。  
先ほどの例のように`property $ \x -> (read . show) x ``shouldBe`` (x + 1 :: Int)`と書いた場合における、関数型`(a -> prop)`のインスタンスは、`(Arbitrary a, Show a, Testable prop) => Testable (a -> prop)`という定義のとおり、関数の戻り値の型が`Testable`のインスタンスでないと、型チェックを通らないはずです。  
`Testable`のインスタンスでない、`IO ()`を返しているにも関わらず型エラーが起きなかったのは、一体なぜでしょうか？

その秘密を探るべく、GHCiを立ち上げましょう。  
先ほどの例のソースコードを`ghci`コマンドに読ませれば、まとめてHspecのモジュールも`import`できるので簡単です。

```bash
> stack exec ghci .\QuickCheck.hs
```

GHCiが起動したら、`:i Testable`と入力して、`Testable`型クラスのインスタンス一覧を出力しましょう。

```haskell
> :i Testable
class Testable prop where
  property :: prop -> Property
  {-# MINIMAL property #-}
        -- Defined in ‘Test.QuickCheck.Property’
instance [safe] Testable Property
  -- Defined in ‘Test.QuickCheck.Property’
instance [safe] Testable prop => Testable (Gen prop)
  -- Defined in ‘Test.QuickCheck.Property’
instance [safe] Testable Discard
  -- Defined in ‘Test.QuickCheck.Property’
instance [safe] Testable Bool
  -- Defined in ‘Test.QuickCheck.Property’
instance [safe] (Arbitrary a, Show a, Testable prop) =>
                Testable (a -> prop)
  -- Defined in ‘Test.QuickCheck.Property’
instance [safe] Testable ()
  -- Defined in ‘Test.QuickCheck.Property’
instance [safe] Testable Test.HUnit.Lang.Assertion
  -- Defined in ‘Test.QuickCheck.IO’
```

ありました！💡
最後の方にある`instance [safe] Testable Test.HUnit.Lang.Assertion`という行に注目してください。  
[`Test.HUnit.Lang.Assertion`](http://hackage.haskell.org/package/HUnit-1.6.0.0/docs/Test-HUnit-Lang.html#t:Assertion)は、`IO ()`の型シノニムであり、Hspecでも間接的に型シノニムとして参照されています[^hspec-expectation]。  
要するに`instance [safe] Testable Test.HUnit.Lang.Assertion`という行は`instance [safe] Testable (IO ())`と読み替えることができます<small>（`[safe]`という表記が指しているもの付いてはここでは省略します！すみません！）</small>。

[^hspec-expectation]: この節の冒頭で「型シノニムのたらい回し」と呼んだものを追いかけてみましょう。  
おなじみ[`shouldBe`](http://hackage.haskell.org/package/hspec-expectations-0.8.2/docs/Test-Hspec-Expectations.html#v:shouldBe)は[`Expectation`](http://hackage.haskell.org/package/hspec-expectations-0.8.2/docs/Test-Hspec-Expectations.html#t:Expectation)という型の値を返します。  
そして`Expectation`は`Assertion`の型シノニムであり、クリックすると[`Test.HUnit.Lang.Assertion`](http://hackage.haskell.org/package/HUnit-1.6.0.0/docs/Test-HUnit-Lang.html#t:Assertion)であることがわかります。  
そして`Assertion`はそう、`type Assertion = IO ()`とあるとおり`IO ()`なのです。やっと知ってる型にたどり着きました😌。

紹介したとおり`Testable`のドキュメントには`Testable Assertion`なんて記載はありませんし、じゃあ一体どこで定義したのか、というとそう、続く行に`-- Defined in ‘Test.QuickCheck.IO’`と書かれているとおり、[`Test.QuickCheck.IO`](https://hackage.haskell.org/package/quickcheck-io-0.2.0/docs/Test-QuickCheck-IO.html)というモジュールで定義されています！  

`Test.QuickCheck.IO`は、名前のとおりQuickCheckの`Testable`について、`IO`のorphan instanceを定義するためのモジュールです。  
これを[`import`している](https://github.com/hspec/hspec/blob/226510631f24b674827e99d17d10f9f92440c5a9/hspec-core/src/Test/Hspec/Core/QuickCheckUtil.hs#L18)が故に、Hspecでは`property`ブロックの中で`shouldBe`などが利用できるんですね！

結論:

- orphan instanceわかりづらい😥
- GHCiの`:i`はorphan instanceであろうとインスタンスを定義した箇所を見つけてくれるから便利！
