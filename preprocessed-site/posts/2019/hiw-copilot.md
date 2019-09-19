---
title: HIW 2019で発表された、Copilotという内部DSLについて
subHeading: ～HIW 2019参加レポート その4～
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: September 24, 2019
tags: Haskell Implementors' Workshop
...
---

[前回](/posts/2019/hiw-gibbon.html)から引き続き、[Haskell Implementors' Workshop 2019](https://icfp19.sigplan.org/home/hiw-2019#About)への参加レポートとして、私の印象に残った発表を紹介します。  
今回は、[Copilot](https://copilot-language.github.io/)という、C言語のコードを生成するHaskell製内部DSLについての発表です。

# Copilot 3.0: a Haskell runtime verification framework for UAVs

発表者: Frank Dedden *Royal Netherlands Aerospace Center*, Alwyn Goodloe *NASA Langley Research Center*, Ivan Perez *NIA / NASA Formal Methods*

Haskell製の内部DSLからC言語のソースコードを生成する、[Copilot](https://copilot-language.github.io/)の紹介です。  
似た謳い文句の内部DSLとして[ivory](http://hackage.haskell.org/package/ivory)がありますが、Copilotは、ハードウェアの実行時検証を行うC言語のコードを生成することに、より特化しています。  
「センサーから信号を受け取って、一定の条件を満たした場合に何らかの処理を実行する」という処理をHaskellで宣言的に記述すると、メモリの消費量・実行時間において常に一定なC言語のコードを生成することが出来ます。

メモリが限られていて、リアルタイムな処理が必要なハードウェアにとって「邪魔にならない監視」を実現するための必須条件なのでしょう。  
現状HaskellはGCが必要であるといった制約もあり、リアルタイムな処理や厳格なメモリー管理が必要な機器での採用は難しいですが、Ivoryや今回発表されたCopilotはあくまでも「C言語のコードを生成するだけ」なので、生成するHaskellではメモリー管理をする必要がありません。  
にっくきスペースリークに悩まされる心配もないのです。  
こういったHaskell製内部DSLは、Haskellの持つ強い型付けによるメリットを享受しながら、変換した言語の実行時におけるパフォーマンスを出しやすい、といういいとこ取りなメリットがあるので、もっと広まってほしいユースケースですね。

# Copilotを試してみる

- ℹ️ 実際に使用したコードは[Haskell-jp BlogのGitHubのリポジトリー](https://github.com/haskell-jp/blog/tree/master/examples/2019/hiw-copilot)にあります。
- ℹ️ 使用したcopilotパッケージのバージョンは、3.0.1です。
- ℹ️ サンプルコードの解説については、notogawaさんのアドバイスも参考になりました<small>（[Haskell-jpのslack-logではこのあたり](https://haskell.jp/slack-log/html/C4M4TT8JJ/46.html#message-1554858057.072700)。執筆時点でCSSが当たってないため読みづらいですが一応）</small>。ありがとうございます！

せっかくなんでCopilotを試してみましょう。  
公式サイトにあったサンプルコードそのまんまですが、生成されるCのコードを眺めてみます。

👇のコマンドでサンプルコードが入ったリポジトリーをgit cloneした後、

```bash
git clone https://github.com/haskell-jp/blog
cd blog/examples/2019/hiw-copilot
```

👇のコマンドでビルド・C言語によるコードの生成できるはずです。

```bash
stack build copilot
stack exec runghc heater.hs
```

こちらが生成元のHaskellのコードです。  

```haskell:heater.hs
import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div)

temp :: Stream Word8
temp = extern "temperature" Nothing

ctemp :: Stream Float
ctemp = (unsafeCast temp) * (150.0 / 255.0) - 50.0

spec = do
  trigger "heaton"  (ctemp < 18.0) [arg ctemp]
  trigger "heatoff" (ctemp > 21.0) [arg ctemp]

main = reify spec >>= compile "heater"
```

まず、`temp`と`ctemp`という識別子に定義した式が、センサーが発信する
Copilotの言葉はこれを`Stream`と呼んでいます。

`spec`という識別子で定義している式が、「どのセンサーから信号を受け取って、どんな条件を満たした場合にどの処理を実行するか」規定しているようです。  
👆の場合、`ctemp`という`Stream`が`18.0`を下回ったら`heaton`というイベントを発火し、`21.0`を超えたら`heatoff`というイベントを発火する、と定めているわけですね。  
そして`main`関数で実行している`reify spec >>= compile "heater"`という箇所で、`.h`ファイルと`.c`ファイルを書き込んでいます。

そして、生成されたヘッダーファイルがこう👇

```c:heater.h
extern uint8_t temperature;
void heatoff(float heatoff_arg0);
void heaton(float heaton_arg0);
void step(void);
```

で、肝心のCのコード本体がこちらです。

```c:heater.c
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include "heater.h"

static uint8_t temperature_cpy;

bool heatoff_guard(void) {
  return ((((float)(temperature_cpy)) * ((150.0) / (255.0))) - (50.0)) > (21.0);
}

float heatoff_arg0(void) {
  return (((float)(temperature_cpy)) * ((150.0) / (255.0))) - (50.0);
}

bool heaton_guard(void) {
  return ((((float)(temperature_cpy)) * ((150.0) / (255.0))) - (50.0)) < (18.0);
}

float heaton_arg0(void) {
  return (((float)(temperature_cpy)) * ((150.0) / (255.0))) - (50.0);
}

void step(void) {
  (temperature_cpy) = (temperature);
  if ((heatoff_guard)()) {
    (heatoff)(((heatoff_arg0)()));
  };
  if ((heaton_guard)()) {
    (heaton)(((heaton_arg0)()));
  };
}
```

先ほど`Stream`として定義した値のうち、`temp`は、`temperature`というグローバル変数と、それを一時的に保存する`temperature_cpy`という二つの変数に翻訳されました。  
`spec`において`trigger`という関数で列挙した「どのセンサーから信号を受け取って、どんな条件を満たした場合にどの処理を実行するか」というルールは、`step`という関数に現れたようです。  
この関数を利用する側では、`heaton`関数と`heatoff`関数を別途定義した上で、`temperature`にセンサーから受け取った値を代入して`step`を呼ぶことによって、`temperature`の値が条件に一致したとき、`heaton`関数と`heatoff`関数を実行してハードウェアの制御ができるのでしょう。  
Haskell側で定義したもう一つの`Stream`、`ctemp`は、`heaton_guard`、`heaton_arg0`、`heatoff_guard`、`heatoff_arg0`、それぞれの関数に書かれた、`temperature_cpy`の値を変換する式に現れているようです。

正直なところこの程度であれば、直接Cで書いた方が余計なカッコもないし読みやすそうではあります。  
`temp`を`ctemp`に変換する式`(150.0 / 255.0) - 50.0`が変換後のソースコードでは冗長に適用されていることから、もっと最適化できそうですし。  
とはいえ、わざわざDSLを作ったからには、より複雑で、Haskellでなければ書いてられないようなケースが、Copilotの開発者の現場ではあるのでしょう<small>（なんせNASAの方も関わっているぐらいですから！）</small>。  
詳しいユースケースや、ビルド時のフローといった運用方法を聞きたいところですね。
