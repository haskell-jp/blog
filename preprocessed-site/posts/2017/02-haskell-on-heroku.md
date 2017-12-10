---
title: Dockerを使ってHaskellアプリをHerokuにデプロイする
headingBackgroundImage: ../../img/post-bg.jpg
headingDivClass: post-heading
heading: Dockerを使ってHaskellアプリをHerokuにデプロイする
subHeading: コンパイル時間に制限されないデプロイ方法
author: Kadzuya Okamoto
postedBy: <a href="https://arow.info#arowM">Kadzuya Okamoto</a>
date: April 30, 2017
tags: Localize, Heroku
...
---

これまで、HaskellのコードをHerokuで実行しようとすると、コンパイルがHerokuの制約時間内に終わらず、面倒なハックが必要でつらい状態でした。
でも、Herokuが[Docker](https://www.docker.com/)をサポートするようになった今なら、Haskell製のウェブアプリケーションをHeroku上で公開するのはずっと簡単です。
この記事では、Servant(HaskellのWebフレームワークの1つ)で作ったアプリケーションを、Dockerの力を借りてHerokuにデプロイする方法について、具体的なプログラムを使って順を追って説明していきます。

## 本記事について

この記事は、[Releasing a Haskell Web App on Heroku with Docker](https://arow.info/blog/posts/2017-03-30-servant-on-heroku.html)としてHaskell-jpオフィシャルスポンサーである[株式会社ARoW](http://arow.info/)公式ブログに公開されている英語の記事を、許可を得て日本版にローカライズしたものです[^me]。

## はじめに

今回、実際にHerokuにデプロイして試せるように、[サンプルアプリ](https://github.com/cdepillabout/servant-on-heroku)を用意しました。
この記事の[最初の章](#dockerを-使わずに-サンプルアプリを実行してみる)では、このサンプルアプリをローカル環境で動かす方法について述べます。
[「今度はDockerをつかってアプリを動かしてみよう！」](#今度はdockerをつかってアプリを動かしてみよう)では、同じくローカル環境において、Dockerを使って動かす方法について触れます。
[「Herokuで動かす」](#herokuで動かす)で、ついにHerokuにこのサンプルアプリをHerokuにデプロイする方法についてお伝えします。

もし、ローカル環境で動かしたりするのが面倒で、「いきなりHerokuにデプロイしたい！」という方は、
[「Herokuで動かす」](#herokuで動かす)から読んでいただいても問題ないように構成しているつもりです。

## Dockerを **使わずに** サンプルアプリを実行してみる

今回用意したサンプルアプリは、以下の通りAPIを2つだけ提供する、とても単純なものです。

* かんたんなコメントのようなものを送信するためのAPI
* これまでに送信された全コメントを表示するためのAPI

このサンプルアプリでは、コメントを保存するのにPostgreSQLを利用しています。

では、まずはDockerやHerokuをつかわないで、実際にローカルな環境でこのアプリをビルドして実行する手順を追っていきましょう。

### ローカル環境でサンプルアプリをビルドする

まず最初に、このサンプルアプリを公開しているgithubレポジトリをcloneして、アプリをビルドしてみましょう。

```sh
$ git clone https://github.com/cdepillabout/servant-on-heroku
$ cd servant-on-heroku/
$ stack setup  # このアプリが使っているバージョンのGHCをインストールします
$ stack build  # 依存パッケージをインストールし、ビルドします
```

もしかしたら、PostgreSQLのライブラリが入っていなくて、ビルドに失敗してしまうかもしれません。

Arch Linuxの場合は、以下のコマンドで必要なライブラリをインストールできます。

```sh
$ sudo pacman -Ss postgresql-libs
```

Ubuntuユーザの方は、以下のコマンドで大丈夫です。

```sh
$ sudo apt-get install libpq-dev
```

上記以外のプラットフォームでは別のコマンドを使うことになると思うので、いい感じにググってください。

では、PostgreSQLの必要なライブラリを入れたところで、`stack build`をもう一度試してみましょう。今度はうまくいきましたよね？

うまくビルドできたら、アプリの実行をしてみます。

```sh
$ stack exec -- servant-on-heroku-api
```

わーお！なにかエラーが出ちゃいますね...

```
servant-on-heroku-api: libpq: failed (could not connect to server: Connection refused
        Is the server running on host "localhost" (::1) and accepting
        TCP/IP connections on port 5432?
        could not connect to server: Connection refused
        Is the server running on host "localhost" (127.0.0.1) and accepting
        TCP/IP connections on port 5432?
)
```

サンプルアプリがPostgreSQLに接続しようとして失敗しているようです。
このアプリは、コメントをPostgreSQLに保存しているので、PostgreSQLがローカルな環境で動いていないと、うまく動きません。

### PostgreSQLのセットアップ

お使いの環境によって、PostgreSQLのインストール方法はまちまちなので、
そのプラットフォームが提供しているドキュメントにしたがって、PostgreSQLのインストールを行ってください。

たとえば、Arch Linuxの場合は[このドキュメント](https://wiki.archlinux.org/index.php/PostgreSQL#Installing_PostgreSQL)です。
Ubuntuなら[ここ](https://help.ubuntu.com/community/PostgreSQL#Installation)にドキュメントがあります。

さて、PostgreSQLをインストールして、動いているのが確認できたら、もう一度アプリを起動してみましょう。

```sh
$ stack exec -- servant-on-heroku-api
```

わーお... またもやエラーです...

```
servant-on-heroku-api: libpq: failed (FATAL:  role "mydbuser" does not exist
)
```

どうやら、このサンプルアプリ用に、PostgreSQLのユーザとデータベースを用意しないといけないようですね。
実際にサンプルアプリのソースコード(`src/Lib.hs`)を見てみると、`DATABASE_URL`という環境変数の値を見てPostgreSQLサーバに接続しているのがわかります。

`DATABASE_URL`環境変数が指定されていない場合は、以下のデフォルト値が使われます。

```
postgres://mydbuser:mydbpass@localhost:5432/mydb
```

`mydbuser`というユーザ名で、`mydbpass`というパスワードを使って`mydb`という名前のデータベースにアクセスしようとしているということですね。
では、実際にこのユーザとデータベースをPostgreSQLで作成してみましょう。
次のコマンドはArch Linuxでしか動かないかもしれません。
もし動かないようであれば、お使いのプラットフォームが提供するドキュメントを参照してください。

最初に、`mydbuser`という名前のユーザを、`mydbpass`というパスワードで作成しましょう。

```sh
$ sudo -u postgres -- psql --command "CREATE ROLE mydbuser NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN ENCRYPTED PASSWORD 'mydbpass'"
```

次に`mydb`という名前のデータベースを作成します。

```sh
$ sudo -u postgres -- createdb mydb
```

`mydbuser`が`mydb`データベースにアクセスできるようにするのも忘れちゃいけませんね。

```sh
$ sudo -u postgres -- psql --command "GRANT ALL PRIVILEGES ON DATABASE mydb TO mydbuser"
```

ここで、PostgreSQLの再起動をしておいた方が無難でしょう。

```sh
$ sudo systemctl restart postgresql
```

これで、実際に`mydb`データベースに、`mydbuser`としてログインすることができるようになったはずです。

```sh
$ psql -U mydbuser -d mydb -h 127.0.0.1
```

### APIを実際にたたいてみる

では、PostgreSQLのセットアップが無事終了したところで、次のコマンドでアプリケーションを立ち上げてみましょう。

```sh
$ stack exec -- servant-on-heroku-api
running servant-on-heroku on port 8080...
```

無事に立ち上がったら、コメントを送ってみます。
アプリが立ち上がった状態で、別のターミナルなどを開いて次のコマンドを打ってみてください。

```sh
$ curl --request POST \
    --header 'Content-Type: application/json' \
    --data '{"author": "DG", "text": "Pretty good"}' \
    'http://localhost:8080/add-comment'
{ "text": "Pretty good", "author": "DG" }
```

よさそうですね！

では、全コメントを取得してみます。

```sh
$ curl --request GET \
    --header 'Content-Type: application/json' \
    'http://localhost:8080/get-comments'
[{"text":"Pretty good","author":"DG"} ]
```

いいですね！ DG (Dennis Gosnell / 原著者)さんが「チョベリグ！」と言っています。
以上で、ローカル環境でアプリを動かすことができたので、次はDockerを使ってみましょう！

## 今度は**Dockerをつかって**アプリを動かしてみよう！

[Docker](https://www.docker.com/)はコンテナ技術を用いたプログラムで、これを使うと仮想環境下でアプリをビルドしたり実際に動かしたりすることができます。
以降では、読者のみなさまがある程度Dockerについて知っている前提で進めていきますが、たぶんそんなによく知らなくても「まぁそんなもんなんだろう」と思いながら読んでいただければ差し支えないと思います。
実際、日本語ローカライズ版を作ってる僕だって、そんなにDockerに詳しいわけではありません。

### Dockerをインストールする

Dockerのインストール方法は環境によってまちまちなので、ご自身の環境に合わせて信用できるドキュメントを参照してください。
[Arch Linuxの場合](https://wiki.archlinux.org/index.php/Docker#Installation)や[Ubuntuの場合](https://docs.docker.com/engine/installation/linux/ubuntu/)はリンク先を読めばなんとかなると思います。

Dockerのインストールが終わったら、以下のコマンドを実行してDockerがちゃんと動いているか確認してみてください。

```sh
$ docker info
```

### Dockerを使ってビルドする

では、実際にDockerを使ってサンプルアプリをビルドし、そのアプリを動かすためのDockerイメージを作成します。

アプリケーションをビルドするには、`docker build`コマンドを使います。

```sh
$ docker build -t servant-on-heroku .
```

このコマンドを実行すると、実行したディレクトリ内に存在する[`Dockerfile`](https://github.com/cdepillabout/servant-on-heroku/blob/master/Dockerfile)という名前のファイルにしたがってアプリをビルドしてくれます。
この`Dockerfile`には、アプリをビルドするための具体的な手順がすべて記述されており、その手続きにしたがって、まったく別の環境でもDockerさえあればアプリを実行できる「イメージ」を作成できます。

ためしに、このサンプルアプリに含まれる`Dockerfile`の中身を見てみましょう。
以下の各処理を実行するようになっています。

1. `apt-get`コマンドを使って、依存パッケージをインストール
2. `stack`をインストール
3. `stack.yaml`を見て、実際に必要なバージョンのGHCを`stack`を使ってインストールする
4. `*.cabal`ファイルの記述にしたがって、アプリが使っているHaskellパッケージをインストールする
5. `stack`を使って実際にアプリをビルドする
6. rootユーザでアプリを実行したくないので、root権限をもつ別のユーザを作成しておく
7. 実際にアプリを実行する

前述した`docker build`コマンドを実行して`servant-on-heroku`という名前のイメージを作成するには1時間近くかかるので[^1]、その間にご飯を食べたり録画しておいたアニメを2本見れます。

### DockerをつかってAPIをテストする

`docker build`が終わったら、`docker images`でローカル環境に存在する全イメージを一覧表示してみましょう。

```sh
$ docker images
REPOSITORY           TAG       IMAGE ID       CREATED         SIZE
servant-on-heroku    latest    ff591d372461   30 seconds ago  3.92 GB
...
```

さきほど作成した`servant-on-heroku`のイメージが作成されているのがわかりますね？

では、`servant-on-heroku`のイメージを走らせてみましょう。次のコマンドを実行すれば、Docker内でこのサンプルアプリが動くはずです。

```sh
$ docker run --interactive --tty --rm servant-on-heroku
```

あぁ... またPostgreSQLの例の問題が出てしまったみたいですね...

```
servant-on-heroku-api: libpq: failed (could not connect to server: Connection refused
        Is the server running on host "localhost" (::1) and accepting
        TCP/IP connections on port 5432?
could not connect to server: Connection refused
        Is the server running on host "localhost" (127.0.0.1) and accepting
        TCP/IP connections on port 5432?
)
```

これはどういうことでしょうか。
`servant-on-heroku`コンテナはDockerコンテナとして動いているため、初期設定では我々のローカル環境が見えず、もちろんローカル環境にセットアップして`localhost:5432`で動いているPostgreSQLも見えないのです。

では、ちょっとしたワザを使ってこの問題を解決してみましょう。
`servant-on-heroku`コンテナを動かしている時に、Dockerに我々のローカル環境のネットワークインタフェースを使うように指示することができます。

```sh
$ docker run --interactive --tty --rm --network host servant-on-heroku
running servant-on-heroku on port 8080...
```

ほら、こうすれば、確かにDockerコンテナからPostgreSQLにアクセスできているようです。

`servant-on-heroku`コンテナが動いている状態で別のシェルを立ち上げて、前の章でやったように`curl`コマンドでAPIが動いているか確かめてみましょう。
まずはコメントの投稿です。

```sh
$ curl --request POST \
    --header 'Content-Type: application/json' \
    --data '{"author": "EK", "text": "Not enough CT"}' \
    'http://localhost:8080/add-comment'
{ "text": "Not enough CT", "author": "EK" }
```

今度はコメントの取得をしてみます。

```sh
$ curl --request GET \
    --header 'Content-Type: application/json' \
    'http://localhost:8080/get-comments'
[{"text":"Pretty good","author": "DG"},{"text":"Not enough CT","author":"EK"}]
```

この通り、無事にEK (Edward Kmett / Haskell界のすごい人)さんが「圏論を、圏論をもっとくれぇええええい！」と言っているコメントが追加されました。

ちなみに、Docker内でシェルを開いて、手動でDockerイメージをいじりながらいろいろ確かめてみるには、次のコメントのようにすればOKです。

```sh
$ docker run --interactive --tty --rm --network host servant-on-heroku /bin/bash
```

では、Docker上でアプリがちゃんと動いたことを確認したところで、ようやくHerokuの出番です。

## Herokuで動かす

Docker上でビルドと実行ができていさえすれば、Herokuにデプロイするのは難しくありません。
まず最初にHerokuのアカウントを作りましょう。

### Herokuアカウントを作成する

Herokuの[アカウント作成ページ](https://signup.heroku.com)でアカウントを作成してください。
もちろん、すでにアカウントを持っているのであればあえて別のアカウントを作りなおす必要はないですよ！

今回はHerokuの無料枠を使ってアプリをデプロイするので、クレジットカードの登録は必要ないです。
こわくないですね！

ここで説明する内容は、ほとんどHerokuの[公式ドキュメント](https://devcenter.heroku.com/articles/container-registry-and-runtime)を参照しているので、なにかわからないところがあったらそちらをチェックしてみてください。

### Herokuのコマンドラインプログラムをインストールする

HerokuはCLIで操作するためのコマンドを用意してくれているので、これを使って便利にHerokuを使い倒せます。
ちょうど、[AWSのCLI](https://aws.amazon.com/cli)とか、[Digital OceanのCLI](https://github.com/digitalocean/doctl)プログラムと同じような感じです。

Arch Linux使いの方は、下記のコマンドでHerokuのCLIプログラムをインストールできます。

```sh
$ yaourt -S heroku-toolbelt
```

このコマンドは、`heroku`コマンドのバイナリを直接取得してインストールしてくれます。

他の環境の方は、[Herokuの公式ドキュメント](https://devcenter.heroku.com/articles/heroku-cli)をご覧ください。

CLIプログラムのインストールができたら、コマンドライン上でログインして、権限を必要とする操作ができる状態にしておきましょう。

```sh
$ heroku login
```

このコマンドを実行すると、ユーザ名とパスワードをたずねられるので、事前に作成しておいたアカウントの情報を入力してください。

### Heroku上でアプリケーションを登録する

今回のサンプルアプリをHerokuで公開するには、まずHeroku上でアプリケーションの登録をする必要があります。

以下のコマンドを実行すると、`servant-on-heroku`という名前のアプリケーションをHerokuに登録できます。
必要に応じて`servant-on-heroku`の部分を別の名前に変更してアプリケーションを登録してください。

```sh
$ heroku apps:create servant-on-heroku
```

以下のコマンドで、いま新規登録されたアプリケーションについての情報を一応取得できます。

```sh
$ heroku apps:info servant-on-heroku
=== servant-on-heroku
Auto Cert Mgmt: false
Dynos:
Git URL:        https://git.heroku.com/servant-on-heroku.git
Owner:          me@gmail.com
Region:         us
Repo Size:      0 B
Slug Size:      0 B
Stack:          cedar-14
Web URL:        https://servant-on-heroku.herokuapp.com/
```

`Web URL`の項目だけ、あとで使うのでどこかにメモしておいてください。
他の項目は、いまは特に気にしなくて大丈夫です。

### Heroku Docker Pluginをインストールする

Herokuのコマンドラインプログラムは、プラグインを追加することで、どんどん便利な機能を使えるようにできます。

今回は、[Heroku Container Registry](https://devcenter.heroku.com/articles/container-registry-and-runtime)というプラグインを使いましょう。

以下のコマンドで、このプラグインがインストールされます。

```sh
$ heroku plugins:install heroku-container-registry
```

インストールが終わったら、次のコマンドを実行して、ちゃんと動いているか確認してください。

```sh
$ heroku container
4.1.1
```

きっと、このプラグインのバージョンナンバーが表示されたはずです。

実際にプラグインを使うためには、以下のコマンドでHeroku Container Registryにログインする必要があります。

```sh
$ heroku container:login
```

このコマンドによって、Container Registryのログイン情報が、`~/.docker/config.json`というファイルに追加されます。

```sh
$ cat ~/.docker/config.json
{
  "auths": {
    "registry.heroku.com": {
      "auth": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx="
    }
  }
}
```

### アプリケーションをHeroku上で動かす

実際にアプリをHeroku上で動かすには、以下のコマンドを使います。

```sh
$ heroku container:push web
```

これを実行すると、実行したディレクトリ内にある`Dockerfile`の設定にしたがってDockerイメージを作成します。
内部では、ローカル環境で`Docker`イメージを作成するときに使ったのと同じ`docker build`を呼んでいます。
前の章で実際に`docker build`を実行した方は、その際に作成したイメージがそのままDocker Container Registryに送られるので安心してください。また1時間も待つなんてイヤですよね。

では、`heroku apps:info`をもう一度実行して確認してみましょう。

```sh
$ heroku apps:info servant-on-heroku
=== servant-on-heroku
Auto Cert Mgmt: false
Dynos:
Git URL:        https://git.heroku.com/servant-on-heroku.git
Owner:          me@gmail.com
Region:         us
Repo Size:      0 B
Slug Size:      0 B
Stack:          cedar-14
Web URL:        https://servant-on-heroku.herokuapp.com/
```

あれ？ なにかおかしいですね... `Dynos:`のところになにも書いてありません。
`dyno`というのはHerokuが独自に使っている用語で、ウェブアプリを実行する1台のサーバのことを意味します。ここになにも書かれていないということは、アプリを実行しているサーバがいないということになります。

これをどうにかするためには、`heroku ps:scale`を使います。

```sh
$ heroku ps:scale web=1
```

これで、"web" dynoが1台分作成され、その上で今回のサンプルアプリが動くようになります。[^3]

では、次のコマンドを実行して、dynoがちゃんと動いていることを確認しましょう。

```sh
$ heroku ps
Free dyno hours quota remaining this month: 549h 2m (99%)
For more information on dyno sleeping and how to upgrade, see:
https://devcenter.heroku.com/articles/dyno-sleeping

=== web (Free): /bin/sh -c /opt/servant-on-heroku/bin/servant-on-heroku-api (1)
web.1: starting 2017/03/22 19:05:04 +0900 (~ 8s ago)
```

なんだか余計な情報もだらだら出てきますが、web dynoが1台分動いていることが確認できます。

これで、サンプルアプリが動くようになったので、`curl`を使って、`Web URL`にアクセスしてみましょう。
(サンプルアプリの`Web URL`は、`heroku apps:info`に書いてありましたよね？)

```sh
$ curl --request POST \
    --header 'Content-Type: application/json' \
    --data '{"author": "MS", "text": "Gotta make it professional"}' \
    'https://servant-on-heroku.herokuapp.com/add-comment'
```

なにかおかしいですね... なにもレスポンスが返ってきません。
なにかエラーが出ているはずなので、Heroku上で起こったエラーを実際に見てみたいです。

### Heroku上で動いているアプリのエラーを見てみる

Herokuには、とってもすばらしいログ機能があり、アプリの標準エラーや標準出力を簡単にチェックできます。

```sh
$ heroku logs
2017-03-22T10:05:49 heroku[web.1]: proc start `/opt/servant-on-heroku/bin/servant-on-heroku-api`
2017-03-22T10:05:52 app[web.1]: servant-on-heroku-api: libpq: failed (could not connect to server: Connection refused
2017-03-22T10:05:52 app[web.1]:    Is the server running on host "localhost" (127.0.0.1) and accepting
2017-03-22T10:05:52 app[web.1]:    TCP/IP connections on port 5432?
2017-03-22T10:05:52 app[web.1]: )
2017-03-22T10:05:52 heroku[web.1]: State changed from starting to crashed
```

とても便利ですね！
どうやらこれまで何度も見てきた例のエラーがまた出ているようです...

今回は、Heroku上で動いているPostgreSQLデータベースをちゃんとセットアップしていないのが理由です。

### HerokuのPostgreSQLサポート

HerokuはPostgreSQLについて[しっかりサポート](https://devcenter.heroku.com/articles/heroku-postgresql)してくれている上に、なんと無料枠まで設けてくれています。

以下のコマンドを実行すれば、PostgreSQLのアドオンが使えるようになります。

```sh
$ heroku addons:create heroku-postgresql:hobby-dev
```

これで、`heroku-postgresql`アドオンを、無料で使える`hobby-dev`利用枠で使えるようになりました。

では、本当にPostgreSQLが作成されたか、以下のコマンドを使って確認してみましょう。

```sh
$ heroku addons:info heroku-postgresql
=== postgresql-tetrahedral-44549
Attachments:  servant-on-heroku::DATABASE
Installed at: Wed Mar 22 2017 19:22:14 GMT+0900 (JST)
Owning app:   servant-on-heroku
Plan:         heroku-postgresql:hobby-dev
Price:        free
State:        created
```

データベースの詳細情報については、`pg:info`コマンドを使って見れます。

```sh
$ heroku pg:info
=== DATABASE_URL
Plan:        Hobby-dev
Status:      Available
Connections: 0/20
PG Version:  9.6.1
Created:     2017-03-22 10:22 UTC
Data Size:   7.2 MB
Tables:      1
Rows:        0/10000 (In compliance)
Fork/Follow: Unsupported
Rollback:    Unsupported
Add-on:      postgresql-tetrahedral-44549
```

### アプリケーションを再起動する

これでPostgreSQLのデータベースが動くようになったので、アプリを再起動しましょう。

```sh
$ heroku ps:restart
```

もう一度ログを見て、本当にこれでエラーが出なくなったか確かめてみます。

```sh
$ heroku logs
2017-03-22T10:22:15 heroku[web.1]: State changed from crashed to starting
2017-03-22T10:22:54 heroku[web.1]: proc start `/opt/servant-on-heroku/bin/servant-on-heroku-api`
2017-03-22T10:22:56 app[web.1]: Migrating: CREATe TABLE "comment"("id" SERIAL8  PRIMARY KEY UNIQUE,"author" VARCHAR NOT NULL,"text" VARCHAR NOT NULL)
2017-03-22T10:22:57 heroku[web.1]: State changed from starting to up
```

すごーい！ついに、ついにちゃんと動いたみたいです！！

もう一度`curl`コマンドを使ってAPIがちゃんと動いているか確認してみます。

```sh
$ curl --request POST \
    --header 'Content-Type: application/json' \
    --data '{"author": "SPJ", "text": "Avoid heroku-at-all-costs"}' \
    'https://servant-on-heroku.herokuapp.com/add-comment'
{"text":"Avoid heroku-at-all-costs","author":"SPJ"}
```

ちゃんとレスポンスが返ってきています！
今度はコメントを取得してみましょう。

```sh
$ curl --request GET \
    --header 'Content-Type: application/json' \
    'https://servant-on-heroku.herokuapp.com/get-comments'
[{"text":"Avoid heroku-at-all-costs","author":"SPJ"}]
```

いいですね！
SPJ(Simon Peyton Jones / Haskellの父)さんが「目先の便利さにとらわれてHerokuに余計な機能をいれるのは、ダメ。ゼッタイ。」[^at-all-costs]と言っています。
これで全てうまくいったようです。

### Heroku上のアプリは接続先のDBをどうやって見つけているのか

賢明な読者のみなさんは、「Heroku上のアプリはどうやってデータベースを見つけているんだろう？」と疑問に思ったかもしれません。
実は、Herokuにはアプリに環境変数を与える仕組みがあります。

この環境変数の設定値を確かめるには、以下のコマンドが使えます。

```sh
$ heroku config
=== servant-on-heroku Config Vars
DATABASE_URL: postgres://someusername:somepassword@ec2-12-12-234-123.compute-1.amazonaws.com:5432/databasename
```

`heroku-postgresql`アドオンでPostgreSQLのデータベースを作成した際に、`DATABASE_URL`という名前の設定値が追加されます。
Herokuはアプリの起動時にこの設定値を環境変数として与えているのです。
先に述べたとおり、今回のサンプルアプリは、`DATABASE_URL`という環境変数を接続先DBの情報として受け取るようになっています[^2]。

Herokuに設定されている環境変数は、`heroku config:get VAR_NAME`で取得できるので、次のコマンドを使ってDBに接続することもできます。

```sh
$ psql "$(heroku config:get DATABASE_URL)"
psql (9.6.1)
SSL connection (protocol: TLSv1.2, cipher: ECDHE-RSA-AES256-GCM-SHA384, bits: 256, compression: off)

databasename=> select * from comment;
 id | author |           text
----|--------|---------------------------
  1 | SPJ    | Avoid Heroku at all costs
(1 row)
```

### アプリのアップデート

Heroku上で動いているアプリをアップデートするのは、とっても簡単です。
単に以下のコマンドを実行するだけで大丈夫です。

```sh
$ heroku container:push web
```

このコマンドは、DockerイメージをビルドしなおしてHeroku container registryにアップします。
その後関係するdynoを全て再起動して、アップデート後のアプリを実行するようにします。

## もっと良くするために

このサンプルアプリは、いまのままでもいい感じですが、いくつかまだ改善の余地があります。
一番手っ取り早い改善箇所は、`Dockerfile`でしょう。
`Dockerfile`をもっと良くするためのアイディアをいくつか挙げてみます。

- `Dockerfile`のベースイメージに、もっとファイルサイズが小さいものを使う

    現状では、[Herokuのイメージ](https://hub.docker.com/r/heroku/heroku/)を使っていますが、
    たぶんもっと軽い[Alpine Linux](https://hub.docker.com/_/alpine/)を使っても問題はないと思います。

- `stack`やGHC、その他よく使うHaskellライブラリが最初から入っているイメージをベースにする

    こうすることで、一番最初の`docker build`に要する時間をガッツリ削ることができます。

- `Dockerfile`の一番最後で、`stack`やGHC、全Haskellライブラリを削除するようにする

    こうすることで、Dockerイメージのサイズを少し減らせる可能性があります。
    Heroku container registryにイメージをアップロードするのが、いくらか早くなるでしょう。

また、`docker-compose`などを使って、ローカルで実行する際にもDockerを使ってPostgreSQL DBをセットアップするのも良いかもしれません。

## まとめ

ローカル環境上でDockerが動いていれば、Heroku上でHaskellのコードを動かすのはとても簡単です。
Herokuの無料枠はアプリのプロトタイプを他の人に試してもらったりするのに最適です。
もちろん、そのままリリースしたら負荷にたえられないかもしれませんが、アプリ開発の最初期段階にコンセプトを検証したりするのには十分でしょう。

もし、検証の結果うまくいきそうだと分かったら、クレジットカードを登録して、
もっと負荷にたえられる有料利用枠でアプリを動かすようにするのだって簡単です。

## 脚注

[^1]: ここで挙げた7ステップはちょっと複雑です。
    もちろん、1コマンドだけでGHCのインストールから依存ライブラリのインストール、
    アプリ自体のビルドまで完了することもできますが、ここではDockerのキャッシュ機構を活用するために
    いくつものコマンドに分けて記述してあります。
    Dockerのキャッシュ機構によって、`docker build`を再実行するときには、入力値が変わったコマンドだけが実行されるようになっています。
    たとえば、`servant-on-heroku.cabal`のファイルを変更して`docker build`を再実行すると、
    `.cabal`ファイルに書かれた依存ライブラリをインストールする(4)のステップからイメージを再ビルドし始めます。
    キャッシュされているデータを利用するので、(1)から(3)までのステップを省略できるのです。

    同じように、`src`下のファイルだけを変更して`docker build`を再実行すると、
    (5)のステップ以降のみが実行されます。
    GHCや依存ライブラリをわざわざ再インストールする必要はないからです。

    このように、ステップをいくつかに分割することで、ビルド時間を大きく節約することができ、
    2回目以降のビルドが数分で終わるようになります。最初のビルドは1時間もかかっていたのに、ちょろいですね。

[^2]: Herokuは、他にも`PORT`という環境変数も使っていて、アプリケーションにどのポートでリクエストを待ち受けるかを指定できます。

[^3]: dynoには[いろいろな種類のもの](https://devcenter.heroku.com/articles/dynos#dyno-configurations)がありますが、
  今回のような単純なWeb APIであれば別にこだわる必要はないです。

[^me]: 僕が自分で許可して、原著者に日本語の内容をチェックしてもらいました。また、翻訳ではなくローカライズなので、原文の逐語訳ではなく、日本語話者にとって理解しやすいように一部加筆修正してあります。

[^at-all-costs]:
元ネタは同氏の["Avoid success at all costs"](https://www.reddit.com/r/haskell/comments/39qx15/is_this_the_right_way_to_understand_haskells/)という言葉で、「目先の便利さにとらわれて、Haskellに余計な機能をいれるのは、ダメ。ゼッタイ。」みたいな意味。
