---
title: CLIアプリのE2Eテストを行うためのライブラリー main-testerをリリースしました
headingBackgroundImage: ../../img/post-bg.jpg
headingDivClass: post-heading
subHeading: たまにはHaskellらしからぬ（？）テストも書いてみよう！
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: April 9, 2018
tags:
...
---

こんにちは。みなさん、テストは書いてますか？  
「[Haskellライブラリ所感2016](http://syocy.hatenablog.com/entry/haskell-library-2016#%E3%83%86%E3%82%B9%E3%83%88)」という記事でも紹介されているとおり、Haskellにも様々なテスト用ライブラリーがあります。  
今回は、「Haskellライブラリ所感2016」でも紹介されている[silently](https://hackage.haskell.org/package/silently)というパッケージにインスパイアされた、新しいテスト用ライブラリーを作りました。  
タイトルにも書きましたが[main-tester](https://hackage.haskell.org/package/main-tester)といいます。

# main-testerができること

main-testerは名前の通り、`main`関数のテストをサポートするライブラリーです。  
Haskell製のプログラムを起動すると最初に実行される、あの`main`関数です。

`main`関数は`IO ()`という型であるとおり、原則として必ず入出力を伴うので、自動テストがしにくい関数です。  
一般的なベストプラクティスとしては、できるだけ`IO`でない、純粋な関数を中心にテストを書いていくのが普通でしょう。  
それでも敢えて`main`関数の自動テストを書くのには、以下のメリットがあります。

1. `main`関数をテストすると言うことは、作っているコマンドの、ユーザーの要求に最も近いレベルのテスト、E2Eテストをすることができる。
1. `main`関数（や、その他の`IO`を伴う関数）に対するテストは、データベースやファイルシステムなど、外部のソフトウェアとの「組み合わせ」で起こるバグを検出できる。
    - 経験上、特に単純なアプリケーションでは、そうした外部のソフトウェアに対する「誤解」が原因となったバグが比較的多いように感じています。
1. 私の個人的な都合ですが、趣味では小さなアプリケーションを書くことが多いので、そうしたE2Eテストの方が効果的だったりする。

このように、`main`関数をはじめとする、`IO`な関数に対して敢えて自動テストを書くことには、様々なメリットがあります。  
`main-tester`はそうした`IO`な関数をテストする際に伴う、2つの問題を解決しました。

1. 標準出力・標準エラー出力に出力した文字列がテストしにくい
    - ➡️ `captureProcessResult`という関数で、標準出力・標準エラー出力に出力した文字列をそれぞれ`ByteString`として取得することができます。
1. 標準入力から文字列を読み出そうとすると、テストの実行が停止してしまう。
    - ➡️ `withStdin`という関数で、標準入力に与えたい文字列を`ByteString`として与えることができます。

ここに書いたことは、ビルドした実行ファイルを子プロセスとして呼び出すことによってもできます。  
入出力の順番など、標準出力や標準エラー出力のより細かい挙動をテストするにはその方がいいでしょう[^thread]。  
しかし、テストのために`PATH`を分離させる必要があったり、そのために[`stack exec`を使ったらめっちゃ遅い](https://github.com/commercialhaskell/stack/issues/2885)という問題があったり、そもそも子プロセス呼び出しはそれだけでオーバーヘッドがあったりと、様々な問題があります。  
物事をよりシンプルにするには、`main`関数を直接呼び出した方がよいでしょう。  
main-testerは、CLIアプリケーションのE2Eテストにおける、そうした子プロセスの呼び出しの問題と、より大きな関数をテストしたいというニーズに応えるためのライブラリーなのです。

[^thread]: `main`関数を子スレッドとして`forkIO`することで同じことが恐らくできますが、テスト結果の報告に使うべき、標準出力・標準エラー出力を食い合うことになってしまうので、非常にやりづらいと思います。

# ほかのライブラリーとの違い

「silentlyというパッケージにインスパイアされた」と冒頭で申しましたとおり、前節で紹介した機能は、実はすでにほかのライブラリーに似たものがあります。  
silentlyに加え、[imperative-edslというパッケージに含まれる、`System.IO.Fake`というモジュール](https://hackage.haskell.org/package/imperative-edsl-0.7.1/docs/System-IO-Fake.html)です<small>（ほかにもあったらすみません！🙇🙇🙇）</small>。  
これらとmain-testerとの違いは何でしょう？

第一に、先ほども触れましたが、main-testerの`captureProcessResult`関数や`withStdin`関数は、標準出力・標準エラー出力・標準入力でやりとりする文字列をstrictな`ByteString`でやりとりします。  
silentlyや`System.IO.Fake`は、`String`なのです。  
`ByteString`は文字通り任意のバイト列を扱うことができるので、「Unicodeの文字のリスト」である`String`よりも、多様なデータを扱うことができます。

これは、特に複数の種類の文字コードを扱うとき、非常に重要な機能となります。  
[以前の記事で取り上げた、`Invalid character`というエラー](https://haskell.jp/blog/posts/2017/windows-gotchas.html)を再現させる場合も、ないと大変やりづらいでしょう。

第二に、main-testerの`captureProcessResult`関数は、`main`関数の終了コードも[`ExitCode`型](https://hackage.haskell.org/package/base-4.11.0.0/docs/System-Exit.html#t:ExitCode)の値として取得できます。  
`main`関数の中で`exitFailure`等の関数を呼び出すと、`ExitCode`が例外として投げられます。  
既存のライブラリーでこれを行うと、`ExitCode`が例外として処理されるため、テストしたい`main`関数の実行が終了してしまいます。  
結果、`main`関数が標準出力・標準エラー出力に書き込んだ文字列を取得することができないのです。  
「○○というエラーメッセージを出力して異常終了する」といったことをテストしたい場合、これでは使いづらいでしょう。  
**「`main`関数のE2Eテストを行うためのライブラリーである」**という観点から、必須の機能であると判断し、実装しました。
ちなみに、`ExitCode`以外の例外についてはそのまま投げられます。仕様を単純にするために、これはユーザーのテストコードの中で処理することとしています。

# 使い方・バグ報告

機能は非常にシンプルなので、使い方については[ドキュメント](https://hackage.haskell.org/package/main-tester-0.1.0.0/docs/Test-Main.html)のサンプルコードを読めば大体わかるかなぁと思いますが、簡単にサンプルを載せておきましょう。

例えばこんなソース👇のプログラムがあった場合、

ExampleMain.hs:

```haskell
module ExampleMain where

import Data.List
import System.Exit

main :: IO ()
main = do
  putStr "What's your name?: "
  name <- getLine
  if "Yuji" `isInfixOf` name
    then putStrLn "Nice name!"
    else die $ name ++ "? Sorry I don't know such a guy!"
```

main-testerを使えば、次のようにHspecでテストできます。

ExampleSpec.hs:

```haskell
import System.Exit
import Test.Main
import Test.Hspec
import qualified ExampleMain
import qualified Data.ByteString as B

main = hspec $
  describe "your-cool-command" $ do
    context "Given 'Yuji' to stdin" $
      it "prints a string including 'Nice name' without an error" $ do
        result <- withStdin "Yuji"$ captureProcessResult ExampleMain.main
        prExitCode result `shouldBe` ExitSuccess
        prStderr result `shouldSatisfy` B.null
        prStdout result `shouldSatisfy` ("Nice name" `B.isInfixOf`)

    context "Given other name to stdin" $
      it "prints an error message" $ do
        result <- withStdin "other name" $ captureProcessResult ExampleMain.main
        prExitCode result `shouldBe` ExitFailure 1
        prStdout result `shouldSatisfy` B.null
        prStderr result `shouldSatisfy` (not . B.null)
```

それぞれのファイルを同じディレクトリーに置いた上で、次のように実行すれば試せるはずです。

```bash
> stack build hspec main-tester
> stack exec runghc -- --ghc-arg=-i. ExampleSpec.hs

your-cool-command
  Given 'Yuji' to stdin
    prints a string including 'Nice name' without an error
  Given other name to stdin
    prints an error message

Finished in 0.0130 seconds
2 examples, 0 failures
```

バグを見つけたら[こちらのGitLabのIssue](https://gitlab.com/igrep/main-tester/issues)に報告してください<small>（最近の個人的な判官贔屓により、敢えてGitLabにしております 😏）</small>。  
それではこの春はmain-testerでHappy Haskell Testing!! 💚💚💚
