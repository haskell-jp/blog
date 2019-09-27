---
title: Haskell Symposium 2019 レポート
subHeading: ""
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Kazuki Okamoto
postedBy: <a href="https://twitter.com/kakkun61">Kazuki Okamoto (@kakkun61)</a>
date: September 28, 2019
tags: ICFP, Haskell Symposium
---

Haskell Symposium 2019にIIJとして参加してきました。

聴講した発表についての概要をまとめましたので、どの論文を読んでみるか決めるなどの際にご活用ください。内容については私の聞きまちがい・読みまちがいなどあると思いますのでご了承ください。

# Bidirectional Type Class Instances

- Koen Pauwels (KU Leuven), Georgios Karachalias (KU Leuven), Michiel Derhaeg (Guardsquare), Tom Schrijvers (KU Leuven)
- <https://icfp19.sigplan.org/details/haskellsymp-2019-papers/8/Bidirectional-Type-Class-Instances>
- <https://arxiv.org/abs/1906.12242>

GADTと型クラスはそれぞれ便利だが混ぜると問題が起きる場合がある。

次のような`Term`があるとき、その`Show`インスタンスを考える。

```haskell
data Term :: Type -> Type where
  Con :: a -> Term a
  Tup :: Term b -> Term c -> Term (b, c)
```

次のように`Show`インスタンスを定義すると型エラーになる。

```haskell
instance Show a => Show (Term a) where
  show (Con a) = show a
  show (Tup x y) = unwords ["(", show x, ",", show y, ")"]
```

```
Could not deduce (Show b) arising from a use of `show'
from the context (Show a) or from (a ~ (b, c))
```

これは`Show (b, c)`ならば`Show b`という関係がないために起こる。

一方タプルについての`Show`は、`Show a`かつ`Show b`ならば`Show (a, b)`という関係である。

```haskell
instance (Show a, Show b) => Show (a, b) where
  …
```

この「ならば」を両方向にすれば問題は解決できるのではないかというのが、この論文の主張である。

# Generic and Flexible Defaults for Verified, Law-Abiding Type-Class Instances

- Ryan Scott (Indiana University), Ryan R. Newton (Indiana University)
- <https://icfp19.sigplan.org/details/haskellsymp-2019-papers/3/Generic-and-Flexible-Defaults-for-Verified-Law-Abiding-Type-Class-Instances>
- <https://ryanglscott.github.io/papers/verified-classes.pdf>

型クラスの法則は依存型を使えば証明できるが、インスタンスごとに書くのはめんどうなので`Generics`で出来るようにしようという話である。

# Modular effects in Haskell through effect polymorphism and explicit dictionary applications - A new approach and the μVeriFast verifier as a case study

- Dominique Devriese (Vrije Universiteit Brussel)
- <https://icfp19.sigplan.org/details/haskellsymp-2019-papers/1/Modular-effects-in-Haskell-through-effect-polymorphism-and-explicit-dictionary-applic>

様々な種類の効果が複雑に絡み合うアプリケーションを整理するために、「効果を伴う処理を持った辞書」を明示的に渡す方式の提案である。

提案した方式によってVeriFastを再実装してみることで、実際に発生した問題と解決方法を解説している。

# Verifying Effectful Haskell Programs in Coq

- Jan Christiansen (Flensburg University of Applied Sciences), Sandra Dylus (University of Kiel), Niels Bunkenburg (University of Kiel)
- <https://icfp19.sigplan.org/details/haskellsymp-2019-papers/4/Verifying-Effectful-Haskell-Programs-in-Coq>
- <https://dl.acm.org/citation.cfm?id=3342592>

Coqによる、効果を伴うプログラムの証明に関する話。

効果について直接証明することはせず、Freeモナドを用いての証明を試みても、そのままCoqに翻訳すると停止性チェックによってエラーになってしまう。

そのために行った工夫に加え、具体例として、`trace`や（部分関数による）エラーなど、Haskellにおいて暗黙に発生する効果を考慮したモデル化について検討した。

# Solving Haskell equality constraints using Coq

- Zubin Duggal
- <https://icfp19.sigplan.org/details/haskellsymp-2019-papers/15/Solving-Haskell-equality-constraints-using-Coq>

data kindsやtype familiesといったGHC拡張によって厳格なデータ型を定義できるが、それに対する操作を定義するとGHCには解けない型レベルの等式が生成されることがある。

制約カインドの型に対する型クラスとして`Proven`を提供し、この制約がある箇所をGHC型検査プラグインが検出して対応するCoqコードのテンプレートを生成する。

そのCoqコードに証明がなければ警告を表示する。

```haskell
type ProofName = Symbol

class c => Proven (prf :: ProofName) (c :: Constraint)
  where {}

applyProof :: forall prf c a. Proven prf c => (c => a) -> a
applyProof x = x

lemma3 = applyProof @"nonzero_pop" @(NNonZero (Popcount b) ~ True) Refl
```

いくつか制約があるがHaskellの型をCoqに自動的に変換している。

# Formal Verification of Spacecraft Control Programs: An Experience Report

- Andrey Mokhov (Newcastle University), Georgy Lukyanov (Newcastle University), Jakob Lechner (RUAG Space Austria GmbH)
- <https://icfp19.sigplan.org/details/haskellsymp-2019-papers/5/Formal-Verification-of-Spacecraft-Control-Programs-An-Experience-Report>
- <https://dl.acm.org/citation.cfm?id=3342593>

REDFINという固定小数演算と整数演算のための処理系があるのだが、そのアセンブリーコードに対して形式検証をしたという報告である。

# G2Q: Haskell Constraint Solving

- William T. Hallahan (Yale University), Anton Xue (Yale University), Ruzica Piskac (Yale University)
- <https://icfp19.sigplan.org/details/haskellsymp-2019-papers/2/G2Q-Haskell-Constraint-Solving>
- <https://dl.acm.org/citation.cfm?id=3342590>

G2QはHaskellのソースにquasi quoteで埋め込むDSLである。

Haskellで書いた条件式をsymbolic executionして、SMT solverに渡す式に変換して、SMT solverに条件を満たす関数を導出させる。

# Making a Faster Curry with Extensional Types

- Paul Downen (University of Oregon), Zachary Sullivan, Zena M. Ariola (University of Oregon), Simon Peyton Jones (Microsoft)
- <https://icfp19.sigplan.org/details/haskellsymp-2019-papers/6/Making-a-Faster-Curry-with-Extensional-Types>
- <https://ix.cs.uoregon.edu/~pdownen/publications/eta.pdf>

パフォーマンスのためにη変換してほしいところを明示したいことがある。

例えば、次のような意味論上は等価な関数`f1`と`f2`があるとする。

```haskell
f1 = \x -> let z = h x x in \y -> e y z
f2 = \x -> \y -> let z = h x x in e y z
```

実際は`f1`は引数`x`を取った後クロージャー生成のためにヒープ確保するのに対して、`f2`はアリティが2の関数と解釈されて中間のクロージャーが必要なくなる。

`~>`というアリティの情報を持った関数型を新たに導入して`->`の代わりに使えるようにする。

`TYPE (a :: RuntimeRep (FunRep 2))`というような新たなポリモーフィズムを導入する。ここでの`2`がアリティ。

`Int`に対して`Int#`があるように基本的にはパフォーマンスが必要なライブラリーなど内部的に使用する想定。

# Multi-Stage Programs in Context

- Matthew Pickering (University of Bristol), Nicolas Wu (Imperial College London), Csongor Kiss (Imperial College London)
- <https://icfp19.sigplan.org/details/haskellsymp-2019-papers/9/Multi-Stage-Programs-in-Context>
- <https://dl.acm.org/citation.cfm?id=3342597>

次のような準引用があったときに、組み合わせると元々あったはずの情報が欠落する場合がある。

```haskell
qshow :: Code (Int -> String)
qshow = [q| show |]

qread :: Code (String -> Int)
qread = [q| read |]

trim :: Code (String -> String)
trim = [q| $(qshow) . $(qread) |]
```

`qshow`と`qread`にあった`Int`という情報が、組み合わせて`trim`とすると欠落してコンパイルエラーになってしまう。

spliceするときにHaskellソースコードの構文木ではなくCoreに対するものを出力すればそれは型が明示されているし問題がない。

しかも、splice後の型検査を省略できるのでコンパイルの高速化にも寄与する。

# Working with Source Plugins

- Matthew Pickering (University of Bristol), Nicolas Wu (Imperial College London), Boldizsár Németh (Eötvös Loránd University)
- <https://icfp19.sigplan.org/details/haskellsymp-2019-papers/11/Working-with-Source-Plugins>
- <https://dl.acm.org/citation.cfm?id=3342599>

souce pluginsのしくみや、書き方、実装時のテクニックの紹介である。

# STCLang: State Thread Composition as a Foundation for Monadic Dataflow Parallelism

- Sebastian Ertel, Justus Adam (Technische Universität Dresden), Norman A. Rink (TU Dresden), Andrés Goens, Jeronimo Castrillon (TU Dresden)
- <https://icfp19.sigplan.org/details/haskellsymp-2019-papers/12/STCLang-State-Thread-Composition-as-a-Foundation-for-Monadic-Dataflow-Parallelism>
- <https://dl.acm.org/citation.cfm?id=3342600>

不聴講

# Synthesizing Functional Reactive Programs

- Bernd Finkbeiner, Felix Klein (Saarland University), Ruzica Piskac (Yale University, Mark Santolucito (Yale University)
- <https://icfp19.sigplan.org/details/haskellsymp-2019-papers/13/Synthesizing-Functional-Reactive-Programs>
- <https://dl.acm.org/citation.cfm?id=3342601>

不聴講

# The essence of live coding: Change the program, keep the state!

- Manuel Bärenz (sonnen eServices GmbH)
- <https://icfp19.sigplan.org/details/haskellsymp-2019-papers/14/The-essence-of-live-coding-Change-the-program-keep-the-state->

不聴講

# Monad Transformers and Modular Algebraic Effects: What Binds Them Together

- Tom Schrijvers (KU Leuven), Maciej Piróg (University of Wrocław), Nicolas Wu (Imperial College London), Mauro Jaskelioff (CONICET)
- <https://icfp19.sigplan.org/details/haskellsymp-2019-papers/7/Monad-Transformers-and-Modular-Algebraic-Effects-What-Binds-Them-Together>
- <https://dl.acm.org/citation.cfm?id=3342595>

モナドトランスフォーマーと代数的効果との対比である。

モナドトランスフォーマーから代数的効果への変換またその逆のときにどういう手法があって、それぞれを構成する要素がどう対応しているかを説明している。

モナドトランスフォーマーと代数的効果だとモナドトランスフォーマーの方が表現できるものが大きいのでモナドトランスフォーマーから代数的効果へはどんなものでも変換できるわけではない。

例えば`catch`や`local`は代数的効果にできない。

# Scoping Monadic Relational Database Queries

- Anton Ekblad (Chalmers University of Technology)
- <https://icfp19.sigplan.org/details/haskellsymp-2019-papers/10/Scoping-Monadic-Relational-Database-Queries>
- <https://dl.acm.org/citation.cfm?id=3342598>

モナドはHaskell界隈で非常に普及しているのでSQLに対するEDSLとしてモナドの構造を採用したい。

このときSQLの結合を表現すると、SQLとしてはスコープ外にもかかわらずEDSLとしてはスコープ内となって使える変数ができてしまう。

これをEDSLとしてもエラーとしたい。

例えば、次のような例で実行時エラーとなってしまう。ここで`a0`は`tableA`の列とする。

```SQL
SELECT a0, b0
FROM
  tableA
    LEFT JOIN
      (SELECT b0 FROM tableB WHERE a0 == b1)
  ON tableA.a2 == tableb.b2
```

`SELECT b0 FROM tableB WHERE a0 == b1`の部分でスコープ外の`a0`を参照しているためエラーとなる。

単純なモナドEDSLだと次のようになりコンパイルが通る。

```haskell
do
  a0 :*: a1 :*: a2 <- from table0
  leftJoin $ do
    b0 :*: b1 :*: b2 <- from table1
    ristrict $ a0 .== b1
  on $ a2 .== b2
```

`ristrict $ a0 .== b1`の部分において`a0`はHaskellとしてはスコープ内にある。

この問題を次のような型レベル関数を駆使することでEDSLにおいてもコンパイル時エラーとすることができた。

```haskell
type family Cols a
type family Outer a
type family UnAggr a
type family FromRow a
```
