---
title: Haskell でも heredoc がしたい
headingBackgroundImage: ../../img/post-bg.jpg
headingDivClass: post-heading
subHeading: string gap の紹介
author: mizunashi_mana
postedBy: mizunashi_mana
tags: heredoc, CPP
date: April 17, 2019
...
---

多くの言語では， here document (heredoc) という言語機能が搭載されています．これは，複数行の文字列をコード中に文字列リテラルとして埋め込める機能です．今日は heredoc ほど使い勝手がよくないものの，長い文字列を埋め込める， Haskell 標準の string gap という機能を紹介したいと思います．

## string gap

bash では，複数行の文字列を，次の記法で埋め込むことができます:

```bash
echo "$(cat <<EOS
some text
is multilined
EOS
)"
```

これは，

```text
some text
is multilined
```

という文字列が出力されます．多くの言語では似たような構文で heredoc が採用されていて，特殊な記号の後に終端記号を書いて，その後の終端記号までを文字列リテラルとして扱われます． Haskell では残念ながらこのような機能は搭載されていませんが，代わりに次の記法が提供されています:

```haskell
main :: IO ()
main = putStrLn "\
  \some text\n\
  \is multilined\
\"
```

この実行結果は，前の bash スクリプトの結果と同じになります． heredoc より色々ごちゃごちゃしてますが，複数行の文字列リテラルを書けます．この機能は， Haskell の複数行文字列リテラルまたは Haskell 標準では gap と呼ばれています [^haskell-string-gap]．記法はかなり単純で，文字列中のバックスラッシュ ``\`` で囲まれた空白が無視されるだけです．改行も空白に含まれます．なので，上のプログラムは以下のプログラムと同じです:

[^haskell-string-gap]: [Haskell2010 の 2.6 節](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6)の最後の方で紹介されています．

```haskell
main :: IO ()
main = putStrLn "some text\nis multilined"
```

なお， gap を使わないで複数行の文字列リテラルを書くことはできません．また， gap は空白を全て無視するため，改行を含まない長い文字列を複数行に渡って埋め込むのにも使えます:

```haskell
main :: IO ()
main = putStrLn "This is very very very \
  \long long long long long long long long text."
```

なお， gap は Haskell 標準でレイアウトルールの処理から除外されているため[^layout-except-gap]，インデントを考慮する必要はありません:

[^layout-except-gap]: [Haskell2010 の 10.3 節](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3) で触れられています．

```haskell
main :: IO ()
main = do
  putStrLn "one line"
  putStrLn "\
\multiline\n\
\text\
\"
```

## CPP 下での注意事項

ただ， GHC の `CPP` 拡張を使用する際注意が必要です． `CPP` では，バックスラッシュで終わる行は，バックスラッシュを除いて次の行と繋げる処理が行われます[^cpp-merge-long-line]．この処理のため， gap を使用した以下のコードは，

[^cpp-merge-long-line]: [CPP の仕様の 1.2 節](https://gcc.gnu.org/onlinedocs/cpp/Initial-processing.html#index-continued-lines) で触れられています．

```haskell
{-# LANGUAGE CPP #-}

main :: IO ()
main = putStrLn "This is very very very \
  \long long long long long long long long text."
```

`cpp` により次のように変換されてしまいます:

```haskell
main :: IO ()
main = putStrLn "This is very very very   \long long long long long long long long text."
```

このため，結果的にコンパイルエラーになってしまいます．このため， `CPP` を使う際は， gap を使わず `CPP` の機能を使う必要があります．例えば，上記のプログラムは，

```haskell
{-# LANGUAGE CPP #-}

main :: IO ()
main = putStrLn "This is very very very \
\ \long long long long long long long long text."
```

と書くと gap をそのまま使った時のプログラムと同じになります．一番最初の `\` は `CPP` のためのもの，次の 2 つは gap になります．

## まとめ

string gap は，昔から Haskell 標準で付いている機能なので，ぜひ使ってみてください．

ただ， heredoc より使い勝手は良くないです．変数展開やもう少し見栄えの良い heredoc が欲しい場合は， [here パッケージ](http://hackage.haskell.org/package/here) や [Shakespeare](http://hackage.haskell.org/package/shakespeare) などの TemplateHaskell を使ったテンプレートエンジンの使用を検討してみるといいかもしれませんね．

では，今日はこれでノシ
