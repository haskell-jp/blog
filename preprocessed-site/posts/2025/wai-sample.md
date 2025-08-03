---
title: 単純なHaskellのみでServant並に高機能なライブラリーを作ろうとした振り返り
subHeading:
headingBackgroundImage: ../img/background.png
headingDivClass: post-heading
author: YAMAMOTO Yuji
postedBy: <a href="http://the.igreque.info/">YAMAMOTO Yuji(@igrep)</a>
date: July 27, 2025
tags:
...
---

この記事では、「[Haskell製ウェブアプリケーションフレームワークを作る配信](https://www.youtube.com/playlist?list=PLRVf2pXOpAzJMFN810EWwGrH_qii7DKyn)」で配信していた、Haskell製ウェブアプリケーションフレームワークを作るプロジェクトについて振り返ります。Servantのような型安全なAPI定義を、<small>（Servantのような）</small>高度な型レベルプログラミングも、<small>（Yesodのような）</small>TemplateHaskellもなしに可能にするライブラリーを目指していましたが、開発を途中で止めることにしました。その振り返り --- とりわけ、そのゴールに基づいて実装するのが困難だと分かった機能などを中心にまとめます。

# 動機

そもそも、Haskellには既にServantやYesod、Scottyといった人気のフレームワークがあるにもかかわらず、なぜ新しいフレームワークを作ろうと思ったのでしょうか。第一に、かつて私が[「Haskellの歩き方」という記事の「Webアプリケーション」の節](https://wiki.haskell.jp/Hikers%20Guide%20to%20Haskell.html#web%E3%82%A2%E3%83%97%E3%83%AA%E3%82%B1%E3%83%BC%E3%82%B7%E3%83%A7%E3%83%B3)で述べた、次の問題を解決したかったから、という理由があります:

> ただしServant, Yesod, 共通した困った特徴があります。
> それぞれがHaskellの高度な機能を利用した独特なDSLを提供しているため、仕組みがわかりづらい、という点です。
> Servantは、「型レベルプログラミング」と呼ばれる、GHCの言語拡張を使った仕組みを駆使して、型宣言だけでREST APIの仕様を記述できるようにしています。
> YesodもGHCの言語拡張をたくさん使っているのに加え、特に変わった特徴として、TemplateHaskellやQuasiQuoteという仕組みを利用して、独自のDSLを提供しています。
> それぞれ、見慣れたHaskellと多かれ少なかれ異なる構文で書かなければいけない部分があるのです。
> つまり、これらのうちどちらかを使う以上、どちらかの魔法を覚えなければならないのです。

この「どちらかの魔法を覚えなければならない」という問題は、初心者がHaskellでウェブアプリケーションを作る上で大きな壁になりえます。入門書に書いてあるHaskellの機能だけでは、ServantやYesodなどのフレームワークで書くコードを理解できず、サンプルコードから雰囲気で書かなければならないのです。これが、新しいフレームワークを作ろうとした一番の動機です。

その他、このフレームワークを開発し始めるより更に前から開発・執筆している、[「失敗しながら学ぶHaskell入門」](https://github.com/haskell-jp/makeMistakesToLearnHaskell/)をウェブアプリケーションとして公開する際のフレームワークとしても使おうという考えもありました。「失敗しながら学ぶHaskell入門」はタイトルの通りHaskell入門者のためのコンテンツです。そのため、Haskellを学習したばかりの人でも簡単に修正できるフレームワークにしたかったのです。

# できたもの

ソースコードはこちら👇️にあります。名前は仮に「wai-sample」としました。

[igrep/wai-sample: Prototype of a new web application framework based on WAI.](https://github.com/igrep/wai-sample)

YouTubeで配信する前から行っていた<small>（私の前職である）</small>IIJの社内勉強会中の開発と、全128回のYouTubeでのライブコーディングを経て<small>（一部配信終了後に手を入れたこともありましたが）</small>、次のような構文でウェブアプリケーションを記述できるようにしました:

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import WaiSample

sampleRoutes :: [Handler]
sampleRoutes =
  [ -- ... 中略 ...

    -- (1) 最も単純な例
  , get @(PlainText, T.Text) "aboutUs" (path "about/us") (\_ -> return "About IIJ")

    -- (2) ステータスコードを指定した例
  , get @(WithStatus Status503 PlainText, T.Text) "maintenance" (path "maintenance")
      (\_ -> return "Sorry, we are under maintenance")

  -- ... 中略 ...

    -- (3) パスをパースして含まれる整数を取得する例
  , get @(PlainText, T.Text)
      "customerTransaction"
      ( (,) <$> (path "customer/" *> decimalPiece)
            <*> (path "/transaction/" *> paramPiece)
        )
      (\(cId, transactionName) ->
        return $ "Customer " <> T.pack (show cId) <> " Transaction " <> transactionName
        )

  -- ... 中略 ...
  ]
```

※完全なサンプルコードは[WaiSample/Sample.hs](https://github.com/igrep/wai-sample/blob/b4ddb75a28b927b76ac7c4c182bad6812769ed01/src/WaiSample/Sample.hs)をご覧ください。上記はその一部に説明用のコメントを加えています。

上記のサンプルコードにおける`sampleRoutes`が、Web APIの仕様を定めている部分です:

```haskell
sampleRoutes :: [Handler]
```

`Handler`という型の値のリストで、それぞれの`Handler`には、Web APIのエンドポイントを表すのに必要な情報が全て含まれています。wai-sampleでは、この`Handler`のリストを解釈してWAIベースのサーバーアプリケーションを実行したり、Template Haskellを通じてクライアントコードを生成したり、はたまたサーバーアプリケーションのドキュメントを生成したりすることができるようになっています。

## (1) 最も単純な例

```haskell
get @(PlainText, T.Text) "aboutUs" (path "about/us") (\_ -> return "About IIJ")
```

先程のサンプルコードから抜粋した最も単純な例↑では、`get`関数を使ってエンドポイントを定義しています。`get`関数は名前のとおりHTTPのGETメソッドに対応するエンドポイントを定義します。`TypeApplications`言語拡張を使って指定している`(PlainText, T.Text)`という型が、このエンドポイントが返すレスポンスの型を表しています。ここでは、`get`に渡す最後の引数に当たる関数（[`Responder`](https://github.com/igrep/wai-sample/blob/b4ddb75a28b927b76ac7c4c182bad6812769ed01/src/WaiSample/Types.hs#L104)と呼びます。詳細は後ほど）がレスポンスボディーとして返す型をお馴染みの`Text`型として指定しつつ、サーバーやクライアントが処理する際はMIMEタイプを`text/plain`として扱うように指定しています。

`get`関数の（値の）第1引数では、エンドポイントの名前を指定しています。この名前は、後述するクライアントコードを生成する機能において、関数名の一部として使われます。

`get`関数の第2引数は、エンドポイントのパスの仕様を表す[`Route`型](https://github.com/igrep/wai-sample/blob/b4ddb75a28b927b76ac7c4c182bad6812769ed01/src/WaiSample/Types.hs#L56-L65)の値です。この例では、`path`関数を使って`"about/us"`という単純な文字列を指定しています。結果、このエンドポイントのパスは`/about/us`となります[^leading-slash]）。

[^leading-slash]: 先頭のスラッシュにご注意ください。wai-sampleが`Route`型の値を処理する際は、先頭のスラッシュは付けない前提としています。

`get`関数の最後の引数が、このエンドポイントがHTTPリクエストを受け取った際に実行する関数、[`Responder`](https://github.com/igrep/wai-sample/blob/b4ddb75a28b927b76ac7c4c182bad6812769ed01/src/WaiSample/Types.hs#L104)です。ここでは、単純にレスポンスボディーとして文字列を返すだけの関数を指定しています。

## (2) ステータスコードを指定した例

```haskell
get @(WithStatus Status503 PlainText, T.Text) "maintenance" (path "maintenance")
  (\_ -> return "Sorry, we are under maintenance")
```

デフォルトでは、`get`関数で定義したエンドポイントはやっぱりステータスコード200（OK）を返します。この挙動を変えるには、先程指定したレスポンスの型のうち、MIMEタイプを指定していた箇所を`WithStatus`型でラップしましょう。型引数で指定しているタプルの1つ目の要素は、このようにHTTPのレスポンスに関する仕様をHaskellの型で指定するパラメーターとなっています。

この例では、`Status503`という型を指定しているため、HTTPステータスコード503（Service Unavailable）を返すエンドポイントを定義しています。

## (3) パスの中に含まれる整数を処理する例

よくあるWebアプリケーションフレームワークでは、パスの一部に含まれる整数など文字列以外の型の値を取得するための仕組みが用意されています。

Haskellにおいて、文字列から特定の型の値を取り出す...といえばそう、パーサーコンビネーターですね。wai-sampleでは、サーバーが受け取ったパスをパーサーコンビネーターでパースするようになっています。従って下記の例では、`/customer/123/transaction/abc`というパスを受け取った場合、`123`と`"abc"`をタプルに詰め込んで`Responder`に渡すパスのパーサーを定義しています:

```haskell
get @(PlainText, T.Text)
  "customerTransaction"
  ( (,) <$> (path "customer/" *> decimalPiece)
        <*> (path "/transaction/" *> paramPiece)
    )
  (\(cId, transactionName) ->
    return $ "Customer " <> T.pack (show cId) <> " Transaction " <> transactionName
    )
```

実際のところここまでの話は`Route`型の値をサーバーアプリケーションが解釈した場合の挙動です。`Route`型はパスの仕様を定義する`Applicative`な内部DSLとなっています。これによって、サーバーアプリケーションだけでなくクライアントのコード生成機能やドキュメントの生成など、様々な応用ができるようになっています。詳しくは後述しますが、例えばクライアントのコード生成機能が`Route`型の値を解釈すると、`decimalPiece`や`paramPiece`などの値は生成した関数の引数を一つずつ追加します。

## Content-Typeを複数指定する

Ruby on Railsの`respond_to`メソッドなどで実現できるように、一つのエンドポイントで一つの種類のレスポンスボディーを、複数のContent-Typeで返す、といった機能は昨今のWebアプリケーションフレームワークではごく一般的な機能でしょう。wai-sampleの場合、例えば次のようにして、`Customer`という型の値をJSONや`application/x-www-form-urlencoded`な文字列として返すエンドポイントを定義できます:

```haskell
sampleRoutes =
  [ -- ... 中略 ...
  , get @(ContentTypes '[Json, FormUrlEncoded], Customer)
  -- ... 中略 ...
  ]
```

これまでの例では`get`の型引数においてMIMEタイプを表す箇所に一つの型のみ（`PlainText`型）を指定していましたが、ここでは代わりに`ContentTypes`という型を使用しています。`ContentTypes`型コンストラクターに、MIMEタイプを表す型の型レベルリストを渡せば、レスポンスボディーを表す一つの型に対して、複数のMIMEタイプを指定できるようになります。

なお、`Json`や`FormUrlEncoded`と一緒に指定した`Customer`型は、当然[`ToJSON`](https://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html#t:ToJSON)・[`FromJSON`](https://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html#t:FromJSON)や[`ToForm`](https://hackage.haskell.org/package/http-api-data/docs/Web-FormUrlEncoded.html#t:ToForm)・[`FromForm`](https://hackage.haskell.org/package/http-api-data/docs/Web-FormUrlEncoded.html#t:FromForm)といった型クラスのインスタンスである必要があります[^http-api-data]。レスポンスボディーとして指定した型が、同時に指定したMIMEタイプに対応する形式に変換できることを、保証できるようになっているのです。

[^http-api-data]: 諸般の事情で、wai-sampleでは[`http-api-data`パッケージをフォーク](https://github.com/igrep/http-api-data/tree/151de32409960354de3a3f786f20bc4a496d2b65)して使っています。そのため、`ToForm`型クラスなどの仕様がHackageにあるものと異なっています。最終的にwai-sampleを公開する際、フォークしたhttp-api-dataを新しいパッケージとして同時に公開する予定でした。

## サーバーアプリケーションとしての使い方

ここまでで定義した`Handler`型の値、すなわちWeb APIのエンドポイントの仕様に基づいてサーバーアプリケーションを実行するには、次のように書きます:

```haskell
import Network.Wai              (Application)
import Network.Wai.Handler.Warp (runEnv)

import WaiSample.Sample         (sampleRoutes)
import WaiSample.Server         (handles)


sampleApp :: Application
sampleApp = handles sampleRoutes


runSampleApp :: IO ()
runSampleApp = runEnv 8020 sampleApp
```

ℹ️[こちら](https://github.com/igrep/wai-sample/blob/b4ddb75a28b927b76ac7c4c182bad6812769ed01/src/WaiSample/Server/Sample.hs)にあるコードと同じ内容です。

`get`関数などで作った`Handler`型のリストを`handles`関数に渡すと、WAIの[`Application`](https://hackage.haskell.org/package/wai-3.2.4/docs/Network-Wai.html#t:Application)型の値が出来上がります。`Application`型はWAIにおけるサーバーアプリケーションを表す型で、ServantやYesodなど他の多くのHaskell製フレームワークでも、最終的にこの`Application`型の値を作るよう設計されています。上記の例は`Application`型の値をWarpというウェブサーバーで動かす場合のコードです。`Application`型の値をWarpの`runEnv`関数に渡すことで、指定したポート番号でアプリケーションを起動できます。

ここで起動したサーバーアプリケーションが、実際にエンドポイントへのリクエストを受け取った際実行する関数は、`get`関数などの最後の引数にあたる関数です。その関数は`SimpleResponder`という型シノニム[^simple]が設定されており、次のような定義となっています:

[^simple]: 名前から察せられるとおり`Simple`じゃない普通の`Responder`型もありますが、ここでは割愛します。`Responder`型はクエリーパラメーターやリクエストヘッダーなど、パスに含めるパラメーター以外の情報を受け取るためのものです。`SimpleResponder`型のすぐ近くで定義されているので、興味があったらご覧ください。

```haskell
type SimpleResponder p resObj = p -> IO resObj
```

ℹ️[こちら](https://github.com/igrep/wai-sample/blob/b4ddb75a28b927b76ac7c4c182bad6812769ed01/src/WaiSample/Types.hs#L106)より

型パラメーター`p`は、エンドポイントのパスに含まれるパラメーターを表す型です。これまでの例で`get`関数に渡した`(path "about/us")`や`((,) <$> (path "customer/" *> decimalPiece) <*> (path "/transaction/" *> paramPiece))`という式で作られる、`Route`型の値を解釈した結果の型`p`です。

そして`resObj`は、エンドポイントが返すレスポンスボディーの型です。これまでの例でいうと、`get`関数の型引数で指定した`(PlainText, T.Text)`における`T.Text`型、`(ContentTypes '[Json, FormUrlEncoded], Customer)`における`Customer`型が該当します。

`runSampleApp`は各`Handler`型の値を解釈し、サーバーアプリケーションとして実行します。エンドポイントのパスの仕様（`(path "about/us")`など）をパーサーコンビネーターとして解釈し[^parser]、パースが成功した`Handler`が持つ`SimpleResponder`（`p -> IO resObj`）を呼び出します。そして`SimpleResponder`が返した`resObj`を、クライアントが要求したMIMEタイプに応じたレスポンスボディーに変換し、クライアントに返す、という流れで動くようになっています。

[^parser]: パーサーコンビネーター以外のアプローチ、例えば基数木を使ってより多くのエンドポイントを高速に処理できるようにするのも可能でしょう。

## Template Haskellによる、クライアントの生成

サーバーアプリケーションの定義だけであれば、Haskell以外のものも含め、従来の多くのウェブアプリケーションフレームワークでも可能でしょう。しかしServantを始め、昨今におけるREST APIの開発を想定したWebアプリケーションフレームワークは、クライアントコードを生成する機能まで備えていることが多いです。wai-sampleはそうしたフレームワークを目指しているため、当然クライアントコードの生成もできるようになっています:

```haskell
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

import WaiSample.Client
import WaiSample.Sample


$(declareClient "sample" sampleRoutes)
```

ℹ️[こちら](https://github.com/igrep/wai-sample/blob/b4ddb75a28b927b76ac7c4c182bad6812769ed01/src/WaiSample/Client/Sample.hs)からほぼそのままコピペしたコードです。

上記の通り、クライアントコードの生成は`TemplateHaskell`を使って行います。`declareClient`という関数に、生成する関数の名前の接頭辞（prefix）とこれまで定義した`Handler`型のリスト（`sampleRoutes`）を渡すと、次のような型の関数の定義を生成します[^ddump-splices]:

```haskell
sampleAboutUs :: Backend -> IO Text
sampleMaintenance :: Backend -> IO Text
sampleCustomerTransaction :: Backend -> Integer -> Text -> IO Text
```

[^ddump-splices]: `ghc`コマンドの`-ddump-splices`オプションを使って、`declareClient`関数が生成したコードを貼り付けました。みなさんの手元で試す場合は`stack build --ghc-options=-ddump-splices`などと実行するのが簡単でしょう。

生成された関数は、`get`関数などの第1引数として渡した関数の名前に、`declareClient`の第1引数として渡した接頭辞が付いた名前で定義されます。

生成された関数の第1引数、`Backend`型は、クライアントがサーバーアプリケーションに実際にHTTPリクエストを送るための関数です。次のように定義されています:

```haskell
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Network.HTTP.Client        as HC

type Backend = Method -> Url -> RequestHeaders -> IO (HC.Response BL.ByteString)
```

このバックエンドを、例えば`http-client`パッケージの関数を使って実装することで、生成された関数がサーバーアプリケーションにリクエストを送ることができます。以下は実際に`http-client`パッケージを使って実装したバックエンドの例です:

```haskell
import qualified Network.HTTP.Client        as HC
import qualified Data.ByteString.UTF8       as BS

httpClientBackend :: String -> Manager -> Backend
httpClientBackend rootUrl manager method pathPieces rawReqHds = do
  req0 <- parseUrlThrow . BS.toString $ method <> B.pack " " <> BS.fromString rootUrl <> pathPieces
  let req = req0 { HC.requestHeaders = rawReqHds }
  httpLbs (setRequestIgnoreStatus req) manager
```

ℹ️[こちら](https://github.com/igrep/wai-sample/blob/b4ddb75a28b927b76ac7c4c182bad6812769ed01/src/WaiSample/Client.hs#L225-L230)からほぼそのままコピペしたコードです。

`Backend`型以外の引数は、パスパラメーターを始めとする、HTTPリクエストを組み立てるのに必要な情報です。`get`関数などで`Handler`型の値を定義する際に指定した`decimalPiece`や`paramPiece`を`declareClient`関数が回収して、生成した関数の引数に追加します。実際に生成した関数が受け取った引数は、もちろんパスの一部として当てはめるのに用います。

生成した関数の戻り値は、サーバーからのレスポンスを表す型です。`get`関数の型引数として渡した`(PlainText, T.Text)`や`(ContentTypes '[Json, FormUrlEncoded], Customer)`などにおける`T.Text`や`Customer`がそれに当たります。クライアントの関数はサーバーからのレスポンスを、MIMEタイプを表す型などに従って、この型に変換してから返すよう実装されているのです。

## ドキュメントの生成

[ServantではOpenAPIに則ったドキュメントを生成するパッケージがある](https://hackage.haskell.org/package/servant-openapi3)ように、Haskellの構文で定義したREST APIの仕様から、APIのドキュメントを生成する機能があると便利でしょう。wai-sampleでも、`Handler`型のリストからAPIのドキュメントを生成する機能を実装しました --- 完成度が低く、とても実用に耐えるものではありませんが。

ともあれ、試しに使ってみましょう。これまで例として紹介した[`sampleRoutes`](https://github.com/igrep/wai-sample/blob/b4ddb75a28b927b76ac7c4c182bad6812769ed01/src/WaiSample/Sample.hs#L147)の各`Handler`に[`showHandlerSpec`](https://github.com/igrep/wai-sample/blob/b4ddb75a28b927b76ac7c4c182bad6812769ed01/src/WaiSample.hs#L47)という関数を適用すると、次のように各エンドポイントへのパスやリクエスト・レスポンスの情報を取得することが出来ます:

```haskell
> mapM_ (TIO.putStrLn . showHandlerSpec) sampleRoutes
index "GET" /
  Request:
    Query Params: (none)
    Headers: (none)
  Response: (PlainText,Text)

maintenance "GET" /maintenance
  Request:
    Query Params: (none)
    Headers: (none)
  Response: ((WithStatus Status503 PlainText),Text)

aboutUs "GET" /about/us
  Request:
    Query Params: (none)
    Headers: (none)
  Response: (PlainText,Text)

-- ... 中略 ...

customerTransaction "GET" /customer/:param/transaction/:param
  Request:
    Query Params: (none)
    Headers: (none)
  Response: (PlainText,Text)

createProduct "POST" /products
  Request:
    Query Params: (none)
    Headers: (none)
  Response: (PlainText,Text)

-- ... 以下略 ...
```

...が、あまりに完成度が低いので、詳しくは解説しません。実際に上記のコード実行すると、`Response`の型などがとても人間に読めるような出力になっていないことが分かります。今どきのWeb APIフレームワークであればOpenAPIに則ったドキュメントを生成する機能が欲しいでしょうが、それもありません。この方向で拡張すれば実装できるとは思いますが、次の節で述べるとおり開発を止めることにしたので、ここまでとしておきます。

# 何故開発を止めるのか

開発をやめる最も大きな理由は、冒頭でも触れたとおり、当初考えていたゴールを達成するのが難しいと判断したからです[^motive]。wai-sampleのゴールは、「Servantのような型安全なAPI定義を、<small>（Servantのような）</small>高度な型レベルプログラミングも、<small>（Yesodのような）</small>TemplateHaskellもなしに可能にするライブラリー」にすることでした。ところが、後述の通りいくつかの機能においてそれが無理ではないか（少なくとも難しい）ということが発覚したのです。

[^motive]: もう1つは、大変申し訳ないですが、私自身のHaskellに対する情熱が落ち込んでしまった、という理由もあります😞。

## 想定通りにできなかったもの: レスポンスに複数のパターンがあるとき

「できたもの」の節では割愛しましたが、wai-sampleでは、サーバーが返すレスポンスに複数のケースがあるエンドポイント --- 例えば、一方ではステータスコード200 OKと共に取得できたリソースの情報を返しつつ、一方では403 Forbiddenと共にエラーメッセージを返す --- の実装もサポートしています。例えば次のように書けば、`/customer/:id.txt`というパスで複数の種類のレスポンスを返すエンドポイントを定義することが出来ます:

```haskell
get @(Sum '[(PlainText, T.Text), Response (WithStatus Status503 PlainText) T.Text])
      "customerIdTxt"
      -- /customer/:id.txt
      (path "customer/" *> decimalPiece <* path ".txt")
      (\i ->
        if i == 503
          then return . sumLift $ Response @(WithStatus Status503 PlainText) ("error" :: T.Text)
          else return . sumLift $ "Customer " <> T.pack (show i))
```

ℹ️[こちら](https://github.com/igrep/wai-sample/blob/b4ddb75a28b927b76ac7c4c182bad6812769ed01/src/WaiSample/Sample.hs#L175-L182)からほぼそのままコピペしたコードです。

`get`関数の型引数に、随分仰々しい型が現れました。`Sum`という型は、名前のとおり和型を作ります。型レベルリストの要素としてContent-Typeやステータスコードを表す型と、実際のレスポンスボディーの型を組み合わせたタプル（あるいは後述する`Response`型）を渡すことで、複数のケースを持つレスポンスの型を定義しています。上記の例における`Sum '[(PlainText, T.Text), Response (WithStatus Status503 PlainText) T.Text]`は、次の2つのケースを持つレスポンスの型を表しています:

- ステータスコードが（デフォルトの）`200 OK`で、Content-Typeが`text/plain`、レスポンスボディーを表す型が`Text`型
- ステータスコードが`503 Service Unavailable`で、Content-Typeが`text/plain`、レスポンスボディーを表す型が`Text`型

以上のように書くことで実装できるようにはしたのですが、これによって当初の目的である「高度な型レベルプログラミングなしに実装する」という目標から外れてしまいました。型レベルリストは「高度な型レベルプログラミング」に該当すると言って差し支えないでしょう。

なぜこのようなAPIになったのかというと、Web APIに対する「入力」に当たる、パスのパース<small>（や、今回は実装しませんでしたがリクエストボディーなどの処理も）</small>などと、Web APIからの「出力」に当たるレスポンスの処理では、実行時に使える情報が大きく異なっていたからです。「入力」は値レベルでも<small>（高度な型レベルプログラミングなしで）</small>Free Applicativeを応用したDSLを使えば[^free-applicative]、サーバーアプリケーション・クライアントコード・ドキュメント、いずれにも実行時に解釈できるフレームワークにできた一方、レスポンスボディーなど「出力」の型は値レベルのDSLを書いても、サーバーアプリケーションを実行しない限りそれに整合しているかどうかが分からない、という原理的な問題が判明したからです。

[^free-applicative]: 今回は詳細を省きましたが`Free Applicative`を使ったDSLの実装は、[WaiSample.Typesモジュール](https://github.com/igrep/wai-sample/blob/b4ddb75a28b927b76ac7c4c182bad6812769ed01/src/WaiSample/Types.hs)をご覧ください。

例えば、レスポンスボディーの仕様を次のような内部DSLで定義できるようにしたとします:

```haskell
get [(plainText, text), ((withStatus status503 plainText), text)]
    -- ...
```

型レベルプログラミングバージョンでは型引数に渡していた情報を、ほぼそのまま値レベルに落とし込んだものです。しかしこのように書いたとしても、サーバーアプリケーションを起動して、実際にクライアントからリクエストを受け取り、それに対して`get`に渡した関数（`Responder`）がレスポンスの元となる値を返すまで、レスポンスボディーの型が正しいかどうか、検証できないのです。「レスポンスの元となる値」の型はライブラリーのユーザー自身が`Responder`で返す値の型ですし、実行時以前にコンパイル時に保証できていて欲しいものです[^launch-time]。これが、値レベルのDSLを採用した場合の限界です。

[^launch-time]: と、ここまで書いて気付いたのですが、`Typeable`型クラスを利用して、サーバーアプリケーション起動時に各`Responder`が返す型が正しいかチェックする、という手法もありだったかもしれません<small>（あるいはそれだけのためにTemplate Haskellをどこかに挟む？）</small>。まあ、型チェックである以上コンパイル時にチェックする方が望ましいでしょうし、私自身のやる気ももう失われてしまったので特に取り組みはしませんが...。


それから、型レベルリストを使ったこと以外においても、複雑で分かりづらい要因があります。
タプル以外にも`Response`という型を

間違い探しみたいな型

https://github.com/igrep/wai-sample/blob/b4ddb75a28b927b76ac7c4c182bad6812769ed01/src/WaiSample/Types/Response.hs#L51-L57

https://github.com/igrep/wai-sample/blob/b4ddb75a28b927b76ac7c4c182bad6812769ed01/src/WaiSample/Sample.hs#L175-L182

## パスのパーサー: 実は`<$>`がすでに危ない

# 実装し切れなかったもの

## よりよいドキュメント生成機能

## よりよいリクエストヘッダー・クエリーパラメーター

# 類似のライブラリー・解決策



開発している途中で見つけた類似のライブラリー

# 終わりに
