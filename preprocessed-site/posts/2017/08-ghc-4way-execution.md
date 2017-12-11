---
title: GHCの４つの実行方法
headingBackgroundImage: ../../img/post-bg.jpg
headingDivClass: post-heading
subHeading: コンパイル実行、スクリプト的実行、対話的実行、ワンライナー的実行
author: takenbu.hs
date: August 13, 2017
...
---

## はじめに

Haskell用コンパイラであるGHCには、以下のように４種類の実行方法があります。

* コンパイル実行 : `ghc`
* スクリプト的実行 : `runghc` (or `runhaskell`)
* 対話的実行 : `ghci`
* ワンライナー的実行 : `ghc -e`

以下、それぞれについて簡単に紹介します。  
なお、本記事では、stack経由ではなく、素のGHCを使う場合について説明しています。 


## コンパイル実行

Haskellのソースファイルから、実行ファイルを生成（コンパイル）する方法です。生成された実行ファイルは、ユーザーが明示的に起動することにより実行されます。

例えばソースファイルが以下の場合に、
```
$ cat prog.hs 

main = print $ 1 + 2
```

以下の様に、`ghc`コマンドを使ってコンパイルを実行できます。
```
$ ghc prog.hs 
[1 of 1] Compiling Main             ( prog.hs, prog.o )
Linking prog ...
```

これにより、実行ファイル（prog）と中間ファイル（prog.hi, prog.o）が生成されます。
```
$ ls
prog   prog.hi  prog.hs  prog.o
```

生成された実行ファイル（prog）は、ユーザーが明示的に指定して起動します。
```
$ ./prog 
3
```

`ghc`のコンパイル方法についての詳しい説明は、[こちら](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using.html)や[こちら](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html)にあります。  
なお、stackを使用する場合は、`ghc`コマンドではなく、`stack ghc`コマンドによりコンパイルを実行できます。



## スクリプト的実行

Haskellのソースファイルから、実行ファイルを一時的に生成（コンパイル）し、その実行ファイルを起動する方法です。つまり、スクリプト的な実行方法です。  
例えば以下の様に、`runghc`コマンド(または別名である`runhaskell`コマンド)により実行できます。
```
$ runghc prog.hs 
3
```

`runghc`の使用方法についての詳しい説明は、[こちら](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runghc.html)にあります。  
なお、stackを使用する場合は、`runghc`コマンドではなく、`stack runghc`コマンドにより実行できます。



## 対話的実行

Haskellのソースを対話的に入力する実行方法です。いわゆる、REPL（read-eval-print-loop）と呼ばれるものです。  
例えば以下の様に、`ghci`コマンドにより実行できます。
```
$ ghci
GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help

Prelude> 1+2
3

Prelude> :q
Leaving GHCi.
```

`ghci`の使用方法についての詳しい説明は、[こちら](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html)にあります。  
なお、stackを使用する場合は、`ghci`コマンドではなく、`stack ghci`コマンドにより実行できます。



## ワンライナー的実行

コマンドライン上で、Haskellの「式」を直接入力する実行方法です。１つの式だけを指定できます。  
例えば以下の様に、`-e`オプション付きの`ghc -e`コマンドにより実行できます。

```
$ ghc -e "1+2"
3
```

`ghc -e`の使用方法についての説明は、[こちら](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using.html#expression-evaluation-mode)にあります。  
なお、stackを使用する場合は、`ghc -e`コマンドではなく、`stack ghc -- -e`コマンドにより実行できます。



## おまけ

以下は、`ghc -e`による実行例です。  
少し特殊かもしれない記法も記載していますので、以下は気軽に読み飛ばしてください。

例えば、このようにして数列を手軽に生成できます。
```
$ ghc -e '[1..10]'
[1,2,3,4,5,6,7,8,9,10]
```

リスト内包表記も使えます。
```
$ ghc -e '[x^2 | x <- [1..10]]'
[1,4,9,16,25,36,49,64,81,100]
```

１行毎に出力することもできます。
```
$ ghc -e 'mapM_ print [1..3]'
1
2
3
```

入力系の関数も使えます。
```
$ ghc -e 'getLine'
ABC
"ABC"
```

入力だけでなく出力系の関数も使えます。
```
$ ghc -e 'getLine >>= putStrLn'
ABC
ABC
```

do記法も使えます。
```
$ ghc -e 'do {x <- getLine; putStrLn x}'
ABC
ABC
```

入出力のフィルタコマンドも表現できます。
```
$ ghc -e 'interact $ unlines . map ("hello " ++) . lines'
John
hello John
Mike
hello Mike
```


ペア形式のデータも簡単に生成できます。
```
$ ghc -e "zip [1..3] ['A'..]"
[(1,'A'),(2,'B'),(3,'C')]
```

組み合わせのデータも簡単に生成できます。
```
$ ghc -e "(,) <$> [1..3] <*> ['A'..'C']"
[(1,'A'),(1,'B'),(1,'C'),(2,'A'),(2,'B'),(2,'C'),(3,'A'),(3,'B'),(3,'C')]
```

組み合わせを連結したデータでも生成できます。
```
$ ghc -e '(++) <$> ["R", "G", "B"] <*> map show [0..3]'
["R0","R1","R2","R3","G0","G1","G2","G3","B0","B1","B2","B3"]
```

ちょっと込み入ったデータも生成できます。
```
$ ghc -e '(\x y -> x ++ "-" ++ y) <$> ["2016", "2017", "2018"] <*> ["Jan", "Feb", "Mar"]'
["2016-Jan","2016-Feb","2016-Mar","2017-Jan","2017-Feb","2017-Mar","2018-Jan","2018-Feb","2018-Mar"]
```

実は ghc -eコマンドでは、ghciのコマンドも使えます。 ですので、型を調べる`:t`(:type)コマンドを使って型の情報を表示させることもできます。
```
$ ghc -e ':t foldl'
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
```

GHC8.2以降であれば、上の例のような一般化された型での表示ではなく、デフォルトの型を考慮してシンプルに表示する`:t +d`コマンドも使えます。（詳細は[こちら](https://downloads.haskell.org/%7Eghc/latest/docs/html/users_guide/ghci.html#ghci-cmd-:type%20+d)）
```
$ ghc -e ':t +d foldl'
foldl :: (b -> a -> b) -> b -> [a] -> b
```

名前の情報を表示する`:i`(:info)コマンドも使えます。
```
$ ghc -e ':i foldl'
class Foldable (t :: * -> *) where
  ...
  foldl :: (b -> a -> b) -> b -> t a -> b
  ...
  	-- Defined in ‘Data.Foldable’
```

カインド（種）を表示する`:k`(:kind)コマンドも使えます。
```
$ ghc -e ':k Maybe'
Maybe :: * -> *
```

Bashなどのシェル環境であれば、シェル変数を使った準クォートも使えます！
```
$ NUM=5
$ ghc -e "[1..$NUM]"
[1,2,3,4,5]
```

シェル側から簡単にデータを渡せます。
```
$ X1="[1..3]"
$ X2="['A'..'C']"
$ ghc -e "zip $X1 $X2"
[(1,'A'),(2,'B'),(3,'C')]
```


interact関数でなく、getContents関数でも入力できます。
```
$ cat test.dat    # data file for example
1
2
3

$ cat test.dat | ghc -e "lines <$> getContents"
["1","2","3"]
```

入力列に対する累算も容易です。
```
$ cat test.dat | ghc -e "(sum . map read .lines) <$> getContents"
6
```

少し記述が長いですが、数値データとして処理させることも出来ます。
```
$ cat test.dat | ghc -e 'interact $ unlines . map (show . (*2) . (read::String -> Int)) . lines'
2
4
6
```

let式も使えます。
```
$ ghc -e "let x = 1; y = 2 in x+y"
3
```

階層指定により特定モジュールの関数を指定することも出来ます。
```
$ ghc -e 'Text.Printf.printf "%s %d\n" "abc" 1'
abc 1
```

`.ghci`ファイルをカレントディレクトリかホームディレクトリに配置しておけば、実行前に読み込めます。
```
$ cat .ghci
import Text.Printf

$ ghc -e 'printf "%s %d\n" "abc" 1'
abc
```

数学関数も使えます。
```
$ ghc -e "sin (pi/2)"
1.0
```

数学関数とリスト内包表記の併用も便利です。
```
$ ghc -e "[sin (n * pi/8) | n <- [0..4]]"
[0.0,0.3826834323650898,0.7071067811865475,0.9238795325112867,1.0]
```

リスト的な処理は、やはり簡単です。
```
$ ghc -e 'replicate 5 "hello"'
["hello","hello","hello","hello","hello"]
```

以上です、enjoy！


