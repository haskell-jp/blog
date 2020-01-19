---
title: リベンジ・オブ・毎時更新 Haskell Antenna
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Nobutada MATSUBARA
postedBy: <a href="https://matsubara0507.github.io/whoami">Nobutada MATSUBARA(@matsubara0507)</a>
date: January 18, 2020
tags: Antenna
...
---

Haskell-jpのコンテンツの一つとして[Haskell Antenna](https://haskell.jp/antenna/)というWebページの開発・運用をしております。

<img src="../../img/2019/hourly-antenna/antenna-page.jpg" style="width: 100%;">

[2019年の今頃、これを自動毎時更新しようと Drone Cloudによる毎時更新を設定しました](https://haskell.jp/blog/posts/2019/hourly-antenna.html)。

しかし。。。なんと去年の3月ぐらいからこれが止まっています（どうやら、[Drone Cloudのこの機能を利用してマイニングをした人がいたらしく止めてしまった](https://discourse.drone.io/t/cron-on-cloud-drone-io/3899/2)ようです）。
現在は**僕がだいたい毎朝1回、手動でCIを回しています**。。。

ずっとなんとかしなきゃなぁと思い続けてはや9ヶ月。
やっと重い腰をあげてなんとかしました！
というよりは、なんとかする方法を思い付いたので実装してみました。

# どうするか？

[GCPにはalways freeプランというのがあり](https://cloud.google.com/free/docs/gcp-free-tier?hl=ja#always-free)、GCEインスタンスの場合はf1-microであれば一台だけ無料です（2020/1現在）。
これに、毎時実行して更新をプッシュするantennaプログラムを仕込んでおけば良いではないかということに気づきました。

Haskell Antenna自体はGitHub Pagesであり、HTMLなどは[haskell-jp/antenna](https://github.com/haskell-jp/antenna)という Haskell製CLIアプリケーションで生成しています。
これをcronか何かで毎時実行すればいいんですけど

1. cronとDockerの組み合わせが割とめんどくさい（antennaはDocker Imageとして提供している）
2. cronにした場合更新をGitHubにどうやってプッシュしようかなどを考えるのがめんどくさい

という問題があります。

そこで、(2) のプッシュの部分も含めて毎時実行の処理をantennaアプリケーションに閉じ込めてしまえば、`docker run` しておくだけで良いのではないか？というのを思い付きました！
ということで、そういう風にantennaを改良します。

# 実装する

antennaプログラムに「gitコマンドを使ってGitHubリポジトリに更新をプッシュする機能」と「全てを毎時実行する機能」の2つを組み込む必要があります。
ここで後方互換性を維持するために、これらはオプションでオンする機能にしましょう。
なのでまずは、antenna CLIアプリケーションのオプションを整理するところから始めます。

## オプションの整理

改修前のantennaはオプションを持っていません。
`getArgs` で引数（設定ファイルのパス）を受け取るだけです

```haskell
import System.Environment (getArgs)

-- generate 関数が設定から HTML ファイル群を生成する IO アクション
main :: IO ()
main = (listToMaybe <$> getArgs) >>= \case
  Nothing   -> error "please input config file path."
  Just path -> generate path =<< readConfig path
```

これを [extensible の `GetOpt`](https://hackage.haskell.org/package/extensible-0.7/docs/Data-Extensible-GetOpt.html) を使ってオプションを貰えるように拡張します

```haskell
-- withGetOpt' は usage を独自で扱えるように拡張した Data.Extensible.withGetOpt です
main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args usage ->
  if | r ^. #help    -> hPutBuilder stdout (fromString usage)
     | r ^. #version -> hPutBuilder stdout (Version.build version)
     | otherwise     -> runCmd r $ listToMaybe args
  where
    opts = #help    @= helpOpt
        <: #version @= versionOpt
        <: #verbose @= verboseOpt
        <: nil

type Options = Record
  '[ "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   ]

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""
```

差分全体はこの[PR](https://github.com/haskell-jp/antenna/pull/20)で確認することができます。
興味のある人はみてみてください。
`generate` 関数は以下の `runCmd` 関数から呼ばれています

```haskell
import Mix
import Mix.Plugin.Logger as MixLogger

runCmd :: Options -> Maybe FilePath -> IO ()
runCmd _ Nothing        = error "please input config file path."
runCmd opts (Just path) = do
  config <- readConfig path
  let plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpts
            <: #config <@=> pure config
            <: nil
  Mix.run plugin $ generate path
  where
    logOpts = #handle  @= stdout
           <: #verbose @= (opts ^. #verbose)
           <: nil
```

`runCmd` 関数は[mix.hs](https://github.com/matsubara0507/mix.hs)を使って `RIO env ()` のボイラーテンプレートを減らしています。

## git コマンドを呼ぶ

Haskellアプリケーションからgitコマンドを実行するには[Shelly](https://hackage.haskell.org/package/shelly)を使うことにします。
Shellyはmix.hsのshellプラグインを使うことで簡単に実装することができます。
まずはコミットを作る部分を実装しましょう

```haskell
import qualified Git -- 自作Shelly製gitコマンド関数群
import qualified Mix.Plugin.Shell as MixShell

runCmd :: Options -> Maybe FilePath -> IO ()
runCmd opts (Just path) = do
  config <- readConfig path
  let plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpts
            <: #config <@=> pure config
            <: #work   <@=> pure "."
            <: nil
  Mix.run plugin $ do
    when (opts ^. #withCommit) $ MixShell.exec (Git.pull [])
    generate path
    when (opts ^. #withCommit) $ commitGeneratedFiles
  where
    logOpts = ...

commitGeneratedFiles :: RIO Env ()
commitGeneratedFiles = do
  files <- view #files <$> asks (gitConfig . view #config)
  MixShell.exec $ do
    Git.add files
    changes <- Git.diffFileNames ["--staged"]
    when (not $ null changes) $ Git.commit ["-m", message]
  where
    message = ...
```

全ての差分はこの[PR](https://github.com/haskell-jp/antenna/pull/21)から確認できます。
`runCmd` 関数に追記したのは `when (opts ^. #withCommit)` から始まる2行です（`Options` に `#withCommit` を追加しています）。
mix.hsのshellプラグインを使うことでShellyのログをだいたいそれっぽくrioのロガーに流してくれます。

次に、`git push`も実装します

```haskell
runCmd :: Options -> Maybe FilePath -> IO ()
runCmd opts (Just path) = do
  ...
  Mix.run plugin $ do
    when (opts ^. #withCommit) $ MixShell.exec (Git.pull [])
    generate path
    when (opts ^. #withCommit) $ commitGeneratedFiles
    when (opts ^. #withPush)   $ pushCommit

pushCommit :: RIO Env ()
pushCommit = do
  branch <- view #branch <$> asks (gitConfig . view #config)
  MixShell.exec (Git.push ["origin", branch])
```

前から使っている `gitConfig` は設定ファイルからgitコマンドに関する設定を取ってきています（例えば、どのファイルをコミットするかやどのブランチにプッシュするかなど）。

これで、差分があった場合は`git commit`を実行し、最後に`git push`するようなオプション、`--with-commit`と`--with-push`を実装できました（他にも実装していますが割愛）。

## 毎時実行

メインディッシュである毎時実行です。
Haskell-jp Slackで、スケジューリング実行をHaskellアプリケーション内で行うのにちょうど良いパッケージはありますか？と尋ねたところ[cron](https://hackage.haskell.org/package/cron)というパッケージを紹介してもらいました（名前がややこしい笑）。
調べてみたところ、ちょうど良さそうなのでこれを使うことにします

```haskell
import System.Cron (addJob, execSchedule)

main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args usage ->
  if | r ^. #help     -> hPutBuilder stdout (fromString usage)
     | r ^. #version  -> hPutBuilder stdout (Version.build version)
     | r ^. #hourly   -> runCmd r (listToMaybe args) `withCron` "0 * * * *"
     | otherwise      -> runCmd r (listToMaybe args)
  where
    opts = ...

withCron :: IO () -> Text -> IO ()
withCron act t = do
  _ <- execSchedule $ addJob act t
  forever $ threadDelay maxBound -- 無限ループ
```

全ての差分はこの[PR](https://github.com/haskell-jp/antenna/pull/22)から確認できます。
すっごい簡単ですね。
ついでに、毎日実行と毎分実行するオプションも追加しています。

これでアプリケーションの方は出来上がったので、こいつをGCEインスタンスで動作させてみましょう。

# インスタンスで起動する

まずはGCP Consoleからインスタンス作成します。
構成は次の通りです

- f1-micro
- オレゴンリージョン
- 30GBの標準ストレージ
- OSはUbuntu 18.04

GCP ConsoleからSSHして、docker コマンドをインストールします（やり方は[公式サイト](https://docs.docker.com/install/linux/docker-ce/ubuntu/)のをそのまま）。
ここまでできたら試しに `sudo docker pull haskelljp/antenna` して最新のイメージを取得してみましょう。

次に、GitHubにプッシュするためにSSH Keyを生成してデプロイキーを haskell-jp/antenna リポジトリに設定します。
できたら適当に `git clone git@github.com:haskell-jp/antenna.git` してブランチを `gh-pages` に切り替えます。

あとは次のコマンドでantennaプログラムを実行するだけです

```
$ sudo docker run -d \
  -v `pwd`:/work
  -v `echo $HOME`/.ssh:/root/.ssh \
  haskelljp/antenna antenna --verbose --with-commit --with-push --with-copy --hourly sites.yaml
```

`docker logs` を使って様子をみてましたが、うまくいってるようです！

# 今後やりたいこと

igrep氏が[Issue](https://github.com/haskell-jp/antenna/issues/16)にしてくれてるように、Haskell Antennaの正しい差分をHaskell-jp Slackに通知する仕組みを整備しようと考えてます。

実はコミットをHaskellアプリケーション内で組み立てるようになった結果、Haskellアプリケーション側でいい感じに差分を調べ上げ、その結果をコミットメッセージに組み込むことができるようになりました。
さすがにHTMLやフィードの `git diff` を解析するのは大変なので、いい感じに各サイトの最終更新ログを残すようにしてみようかなって考えてます。
