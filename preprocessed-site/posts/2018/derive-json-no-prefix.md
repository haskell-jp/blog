---
title:  deriveJsonNoPrefixをリリースしました
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
subHeading: レコードラベルにprefixを着けざるを得ない人達に送るライブラリーです
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: July 18, 2018
tags:
...
---

前回の更新からちょっと時間が空いてしまいました 💦  
小ネタです。掲題の通り[deriveJsonNoPrefix](http://hackage.haskell.org/package/deriveJsonNoPrefix)というパッケージをリリースしました。  
地味に有用だと思うので、[README](https://gitlab.com/igrep/deriveJsonNoPrefix/blob/master/README.md)をやや意訳気味に翻訳して記事にします。  
十分に単純なので、仕様が変わることもまさかないでしょうし。

以下、[こちらのコミットの時点のREADME](https://gitlab.com/igrep/deriveJsonNoPrefix/blob/6114e0fc55cf5b57a771871e53971a51592f618b/README.md)の翻訳です。

# deriveJsonNoPrefix

プレフィックスに優しい`ToJSON`と`FromJSON`のインスタンスを定義するTemplate Haskellのマクロを提供します。

## 例

こんな感じのJSONを作りたいとしましょう:

```json
{
    "id": "ID STRING",
    "max": 0.789,
    "min": 0.123
}
```

きっと[ToJSON](http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:ToJSON)（おそらくそれに加えて[FromJSON](http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:FromJSON)も）のインスタンスを自動的に定義するための、次のようなレコード型を定義したくなるでしょう。

```hs
{-# LANGUAGE TemplateHaskell #-}

import Data.Aeson.TH

data SomeRecord = SomeRecord
  { id :: String
  , max :: Double
  , min :: Double
  } deriving (Eq, Show)

$(deriveToJSON defaultOptions ''SomeRecord)
```

しかし、こんなレコード型は定義すべきではありません。  
`id`も`max`も`min`も、`Prelude`に定義済みなのですから！

この問題を回避するために、レコードラベルに型の名前をプレフィックスとして加える、ということをわれわれはよくやります。

```hs
data SomeRecord = SomeRecord
  { someRecordId :: String
  , someRecordMax :: Double
  , someRecordMin :: Double
  } deriving (Eq, Show)
```

そして、`deriveToJSON`にデフォルトと異なるオプションを渡して実行します。

```hs
deriveToJSON Json.defaultOptions { fieldLabelModifier = firstLower . drop (length "SomeRecord") } ''SomeRecord

firstLower :: String -> String
firstLower (x:xs) = toLower x : xs
firstLower _ = error "firstLower: Assertion failed: empty string"
```

`fieldLabelModifier`オプションは文字通り、対象のレコードをJSONに変換するとき、あるいはJSONから対象のレコードの値に変換する時、レコードのラベルを変換する関数を設定するために使います。  
👆の場合、プレフィックスである`SomeRecord`の文字数分レコードラベルから`drop`して、先頭の文字<small>（`someRecordId`で言えば`Id`の`I`に相当します）</small>を小文字に変換しているのがわかるでしょうか？

そう、これが`deriveToJsonNoTypeNamePrefix`がやっていることとほぼ同等のことです。  
`deriveToJsonNoTypeNamePrefix`は、実質次のように定義されています。

```hs
deriveToJsonNoTypeNamePrefix :: Name -> Q [Dec]
deriveToJsonNoTypeNamePrefix deriver name =
  deriveToJSON Json.defaultOptions { fieldLabelModifier = dropPrefix name } name

dropPrefix :: Name -> String -> String
dropPrefix name = firstLower . drop (length $ nameBase name)

firstLower :: String -> String
firstLower (x:xs) = toLower x : xs
firstLower _ = error "firstLower: Assertion failed: empty string"
```

結果、これからは`fieldLabelModifier`をもう自分で定義する必要がありません！🙌

```hs
import Data.Aeson.DeriveNoPrefix

$(deriveJsonNoTypeNamePrefix ''SomeRecord)
```

👆 の`deriveJsonNoTypeNamePrefix` は [deriveJSON](https://hackage.haskell.org/package/aeson-1.3.1.0/docs/Data-Aeson-TH.html)と同様に、`ToJSON`と`FromJSON`のインスタンス、両方を生成します。  
もちろん、`FromJSON`のインスタンスを生成するときのオプションとしても、プレフィックスを削除するための`fieldLabelModifier`を渡してくれます！

## 同じ問題を解決するほかのライブラリー

- [extensible](https://hackage.haskell.org/package/extensible).
- など、`ToJSON`・`FromJSON`のインスタンスが定義されたextensible recordを提供するライブラリー

なので、そうしたextensible recordを提供するライブラリーが学習コストや依存関係などの事情で「重たい」と感じたときにこのパッケージを使ってください。
