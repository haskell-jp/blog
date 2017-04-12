---
title: Haskell-jp Blog -- Dockerを使ってHaskellアプリをHerokuにデプロイする
headingBackgroundImage: ../img/post-bg.jpg
headingDivClass: post-heading
heading: HaskellアプリをHerokuにデプロイするナウい方法
subHeading: Dockerを使ってコンパイル時間に制限されないデプロイ
postedBy: <a href="https://arow.info#arowM">Kadzuya Okamoto</a>
draft: true
---

Herokuが[Docker](https://www.docker.com/)をサポートするようになって、Haskell製のウェブアプリケーションをHeroku上で公開するのがずいぶんと楽になりました。
この記事では、Servant(HaskellのWebフレームワークの1つ)で作ったアプリケーションを、Dockerの力を借りてHerokuにデプロイする方法について、具体的なプログラムを使って順を追って説明します。

## 本記事について

この記事は、[Releasing a Haskell Web App on Heroku with Docker](https://arow.info/blog/posts/2017-03-30-servant-on-heroku.html)としてHaskell-jpオフィシャルスポンサーである[株式会社ARoW](http://arow.info/)公式ブログに公開されている英語の記事を、許可を得て日本版にローカライズしたものです[^me]。

## はじめに

今回、実際にHerokuにデプロイして試せるように、[サンプルアプリ](https://github.com/cdepillabout/servant-on-heroku)を用意しました。
この記事の最初の章では、このサンプルアプリをローカル環境で動かす方法について述べます。
第2章では、同じくローカル環境において、Dockerを使って動かす方法について触れます。
第3章で、ついにHerokuにこのサンプルアプリをデプロイする方法についてお伝えします。

もし、ローカル環境で動かしてみたりしないで、いきなりHerokuにデプロイしたい方は、第3章から読んでいただいても問題ありません。

## Dockerを **使わずに** サンプルアプリを実行してみる

今回用意したサンプルアプリは、APIを2つだけ提供する、とても単純なものです。

* かんたんなコメントのようなものを送信するためのAPI
* これまでに送信された全コメントを表示するためのAPI

このサンプルアプリは、コメントを保存するのにPostgreSQLを利用しています。

では、まずはDockerやHerokuをつかわないで、実際にローカルな環境でこのアプリをビルドして実行する手順を追っていきましょう。

### ローカル環境でサンプルアプリをビルドする

まず最初に、サンプルアプリを公開しているgithubレポジトリをcloneして、アプリをビルドしてみましょう。

```sh
$ git clone https://github.com/cdepillabout/servant-on-heroku
$ cd servant-on-heroku/
$ stack setup  # このアプリが使っているバージョンのGHCをインストールします
$ stack build  # 依存パッケージをインストールし、ビルドします
```

もしかしたら、PostgreSQLのライブラリが入っていなくて、ビルドに失敗してしまうかもしれません。

もしArch Linuxを使っているのであれば、以下のコマンドで必要なライブラリをインストールできます。

```sh
$ sudo pacman -Ss postgresql-libs
```

Ubuntuユーザの方は、以下のコマンドです。

```sh
$ sudo apt-get install libpq-dev
```

上記以外のプラットフォームでは別のコマンドを使うことになると思います。

では、PostgreSQLの必要なライブラリを入れたところで、`stack build`をもう一度試してみましょう。今度はうまくいくはずです。

さぁ、アプリの実行をしてみましょう。

```sh
$ stack exec -- servant-on-heroku-api
```

わーお！ 以下のエラーが出ちゃいますね...

```
servant-on-heroku-api: libpq: failed (could not connect to server: Connection refused
        Is the server running on host "localhost" (::1) and accepting
        TCP/IP connections on port 5432?
        could not connect to server: Connection refused
        Is the server running on host "localhost" (127.0.0.1) and accepting
        TCP/IP connections on port 5432?
)
```

サンプルアプリがPostgreSQLに接続しようとして失敗しています。
このアプリは、コメントをPostgreSQLに保存しているので、PostgreSQLがローカルな環境で動いていないといけません。

### PostgreSQLのセットアップ

OSやディストリビューションによって、PostgreSQLのインストール方法はまちまちです。
実際にお使いのプラットフォームが提供しているドキュメントにしたがって、PostgreSQLのインストールを行ってください。

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

無事に立ち上がったら、コメントを送ってみましょう。
アプリが立ち上がった状態で、別のターミナルなどを開いて次のコマンドを打ってみてください。

```sh
$ curl --request POST \
    --header 'Content-Type: application/json' \
    --data '{"author": "DG", "text": "Pretty good"}' \
    'http://localhost:8080/add-comment'
{ "text": "Pretty good", "author": "DG" }
```

では、全コメントを取得してみます。

```sh
$ curl --request GET \
    --header 'Content-Type: application/json' \
    'http://localhost:8080/get-comments'
[{"text":"Pretty good","author":"DG"} ]
```

いいですね！ DG (Dennis Gosnell / 原著者)さんが「チョベリグ！」と言っています。
ローカル環境でアプリを動かすことができたので、次はDockerを使ってみましょう！

## 今度は**Dockerをつかって**アプリを動かしてみよう！

[Docker](https://www.docker.com/)はコンテナ技術を用いていて、これを使うと仮想環境下でアプリをビルドしたり実際に動かしたりすることができます。
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
この`Dockerfile`には、アプリをビルドするための具体的な作業がすべて記述されており、その手続きにしたがってDockerがまったく別の環境でもDockerさえあればアプリを実行できる「イメージ」を作成します。

ためしに、このサンプルアプリに含まれる`Dockerfile`の中身を見てみましょう。
どうやら、以下の各処理を実行するようになっているようです。

1. `apt-get`コマンドを使って、依存パッケージをインストール
2. `stack`をインストール
3. `stack.yaml`を見て、実際に必要なバージョンのGHCを`stack`を使ってインストールする
4. `*.cabal`ファイルの記述にしたがって、アプリが使っているHaskellパッケージをインストールする
5. `stack`を使って実際にアプリをビルドする
6. rootユーザでアプリを実行したくないので、root権限をもつ別のユーザを作成しておく
7. 実際にアプリを実行する

前述した`docker build`コマンドを実行して`servant-on-heroku`という名前のイメージを作成するのには、1時間近くかかるので[^1]、その間にご飯を食べたり録画しておいたアニメを2本見れます。

### DockerをつかってAPIをテストする

`docker build`が終われば、`docker images`でローカル環境に存在する全イメージを一覧にして表示できます。

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

## Heroku

Once we have the application building and running successfully in Docker, it's
easy to move to Heroku. The first step is creating a Heroku account.

### Creating an Account

Go [here](https://signup.heroku.com) to sign up for a Heroku account. If you
already have a Heroku account, you can skip this step.

We will deploy out application using Heroku's "Free" tier, so you don't need to
worry about registering a credit card.

The majority of the instructions in this section are condensed from
Heroku's
[own documentation](https://devcenter.heroku.com/articles/container-registry-and-runtime) on
integrating with Docker. Check out their documentation is anything is unclear.

### Install the Heroku CLI Application

Heroku provides a CLI application to make it easy to work with their service.
This is similar to [AWS's CLI](https://aws.amazon.com/cli)
or [Digital Ocean's CLI](https://github.com/digitalocean/doctl).

On Arch Linux, Heroku's CLI application can be installed with the following
command:

```sh
$ yaourt -S heroku-toolbelt
```

This installs the `heroku` binary to the system.

Instructions for other platforms can be found
on [Heroku's site](https://devcenter.heroku.com/articles/heroku-cli).

Once the CLI application has been downloaded, it can be used to login and
authenticate with Heroku's API:

```sh
$ heroku login
```

You will be asked for the username and password of the account you just created.

### Create an Application on Heroku

The first step of releasing our Servant API to Heroku is to create a Heroku
Application.

The following command will create a new Heroku application called
`servant-on-heroku`. You may need to use a different name for your own
application:

```sh
$ heroku apps:create servant-on-heroku
```

The following command lists information about the application just created
(although it won't be too interesting yet):

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

Make sure to take note of the `Web URL`. It will come in handy later.

### Install Heroku Docker Plugin

The Heroku CLI application has a plugin architecture. This allows the user to
install plugins that can be used to access different parts of Heroku's API.

There is a plugin for using
Heroku's
[Docker Container Registry](https://devcenter.heroku.com/articles/container-registry-and-runtime).

The following command can be used to install the plugin:

```sh
$ heroku plugins:install heroku-container-registry
```

After installing the plugin, the following command can be used to make sure it
works:

```sh
$ heroku container
4.1.1
```

It should return the version string for the plugin.

In order to actually use the plugin, the following command can be used to login
to Heroku's container registry.

```sh
$ heroku container:login
```

This adds login information for Heroku's container registry to the file
`~/.docker/config.json`:

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

### Get the Application Running on Heroku

In order to get the application actually running on Heroku, the following
command is used:

```sh
$ heroku container:push web
```

This builds a Docker image for the application based on the `Dockerfile` in the
current directory. Internally, `docker build` is used to do this. If the image
was already built in the previous step (when running `docker build` from the
command line), then this `heroku container:push` command will just use the
previously built image. The image is sent to Docker's Container Registry.

Now let's check `heroku apps:info` again:

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

Hmm, that's not right. See where it says `Dynos: `? A "dyno" is Heroku-lingo for
a server that runs the web application. This line means that there aren't any
servers running the application.

In order to fix this, the `heroku ps:scale` command can be used to spin up one
dyno to run the application:

```sh
$ heroku ps:scale web=1
```

This creates one "web" dyno, which will run the Servant API.[^3]

Now run the following command to make sure the dyno is actually running:

```sh
$ heroku ps
Free dyno hours quota remaining this month: 549h 2m (99%)
For more information on dyno sleeping and how to upgrade, see:
https://devcenter.heroku.com/articles/dyno-sleeping

=== web (Free): /bin/sh -c /opt/servant-on-heroku/bin/servant-on-heroku-api (1)
web.1: starting 2017/03/22 19:05:04 +0900 (~ 8s ago)
```

The output is somewhat noisy, but you can tell that there is now one web dyno
running.

Now that the application is running, the following command can be used to access
the application's `Web URL` with curl. (The application `Web URL` can be found in
the output of `heroku apps:info`.)

```sh
$ curl --request POST \
    --header 'Content-Type: application/json' \
    --data '{"author": "MS", "text": "Gotta make it professional"}' \
    'https://servant-on-heroku.herokuapp.com/add-comment'
```

That's strange, there appears to be another error. Let's see how to investigate
application errors on Heroku.

### Debugging Application Errors

Heroku has a really nice log system. The application's `stdout` and `stderr`
logs can be inspected with the following command:

```sh
$ heroku logs
2017-03-22T10:05:49 heroku[web.1]: proc start `/opt/servant-on-heroku/bin/servant-on-heroku-api`
2017-03-22T10:05:52 app[web.1]: servant-on-heroku-api: libpq: failed (could not connect to server: Connection refused
2017-03-22T10:05:52 app[web.1]:    Is the server running on host "localhost" (127.0.0.1) and accepting
2017-03-22T10:05:52 app[web.1]:    TCP/IP connections on port 5432?
2017-03-22T10:05:52 app[web.1]: )
2017-03-22T10:05:52 heroku[web.1]: State changed from starting to crashed
```

Oh no! It's the same error that has been plaguing us this whole time. Why is it
occurring again?

Well, it's because we haven't setup a PostgreSQL database on Heroku!

### PostgreSQL on Heroku

Heroku
has [nice support](https://devcenter.heroku.com/articles/heroku-postgresql) for
PostgreSQL. Heroku provides a PostgreSQL database that can be used
free-of-charge.

The following command can be used enable the PostgreSQL database add-on for the
application:

```sh
$ heroku addons:create heroku-postgresql:hobby-dev
```

This enables the `heroku-postgresql` add-on in the `hobby-dev` tier (which is
free).

After enabling it, the following command can be used to make sure the PostgreSQL
database has been successfully created:

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

The database info can be checked with the `pg:info` command:

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

### Restart the App

Now that the PostgreSQL database is up and running, let's try restarting the
application:

```sh
$ heroku ps:restart
```

Let's take a look at the application logs again:

```sh
$ heroku logs
2017-03-22T10:22:15 heroku[web.1]: State changed from crashed to starting
2017-03-22T10:22:54 heroku[web.1]: proc start `/opt/servant-on-heroku/bin/servant-on-heroku-api`
2017-03-22T10:22:56 app[web.1]: Migrating: CREATe TABLE "comment"("id" SERIAL8  PRIMARY KEY UNIQUE,"author" VARCHAR NOT NULL,"text" VARCHAR NOT NULL)
2017-03-22T10:22:57 heroku[web.1]: State changed from starting to up
```

Looks like it worked this time!  Finally!

Let's try accessing the app using `curl` again:

```sh
$ curl --request POST \
    --header 'Content-Type: application/json' \
    --data '{"author": "SPJ", "text": "Avoid heroku-at-all-costs"}' \
    'https://servant-on-heroku.herokuapp.com/add-comment'
{"text":"Avoid heroku-at-all-costs","author":"SPJ"}
```

And once more:

```sh
$ curl --request GET \
    --header 'Content-Type: application/json' \
    'https://servant-on-heroku.herokuapp.com/get-comments'
[{"text":"Avoid heroku-at-all-costs","author":"SPJ"}]
```

Success! Looks like everything is working well!

### How does the app on Heroku know how to connect to the database?

You may be wondering how the application running on Heroku knows how to
connect to the database. Well, Heroku has configuration variables that it passes
to the application as environment variables.

These configuration variables can be inspected with the following command:

```sh
$ heroku config
=== servant-on-heroku Config Vars
DATABASE_URL: postgres://someusername:somepassword@ec2-12-12-234-123.compute-1.amazonaws.com:5432/databasename
```

Setting up the PostgreSQL database creates a configuration variable
called `DATABASE_URL`. Heroku passes this configuration variable to the
application on startup as an environment variable. As discussed in a previous
section, the application uses `DATABASE_URL` to connect to the correct
database[^2].

Heroku's `DATABASE_URL` can also be used to connect to the database on the
command line:

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

### Future (Normal) Releases

Performing future releases of the application is extremely easy. Just run the
following command:

```sh
$ heroku container:push web
```

This rebuilds the docker image for the application and pushes it to Heroku's
container repository. It then restarts the dynos so they are running with the
new code for the application.

## Future Work

This application works pretty well, but there are a couple places for
improvements. The lowest hanging fruit would probably be the `Dockerfile`.
Here are a couple ideas that would make the `Dockerfile` a little better:

- Use a slimmer image as the base image for the `Dockerfile`. Right now it is
  using [Heroku's images](https://hub.docker.com/r/heroku/heroku/), but I don't
  think there is any reason that something like
  [Alpine Linux](https://hub.docker.com/_/alpine/) couldn't be used.
- Base the image on something with `stack`, GHC, and popular Haskell libraries
  already installed. This would greatly reduce the time it takes to do the very
  initial `docker build`.
- At the very end of the `Dockerfile`, remove `stack`, GHC, and all Haskell
  libraries. This would hopefully make the docker image a little smaller. It
  would take less bandwidth to send the image to Heroku's container repository.

It would also be nice to use something like `docker-compose` to setup the
PostgreSQL database using Docker when running locally.

## Conclusion

As long as you have Docker running on your local machine (and maybe PostgreSQL
for testing), it's pretty easy to get your Haskell code on Heroku. Heroku's free
plan is nice for testing application ideas and showing them to others. It may
not work for any sort of business application, but as a proof-of-concept, it's
great!

If you decide your proof-of-concept works well and you want to release it, it's
easy to add a credit card to Heroku and start running on their cheapest paid
tier.  It is a very easy upgrade path.

## 脚注

[^1]: These seven steps are slightly complicated. Ideally, it should be possible
    to install GHC, install all the application dependencies, and build the
    application in just one command. However, I have separated it into multiple
    commands to take advantage of Docker's caching ability. When re-running
    `docker build`, only commands where the input has changed will be re-run.

    For example, if you change the `servant-on-heroku.cabal` file and re-run
    `docker build`, it will rebuild the image from (4), starting with installing
    dependencies from the application's `.cabal` file. `docker build` does not
    have to re-run (1), (2), or (3). It uses cached versions of the image.

    This means that if all you change is the application source code under
    `src/` and re-run `docker build`, all `docker build` has to do is re-run
    (5), (6), and (7). It doesn't have to install GHC or the application's
    Haskell dependencies. This reduces a large part of the build-time. Future
    builds will take just a few minutes, instead of one hour.

[^2]: Heroku also makes use of the `PORT` environment variable for telling your
    application which port to listen on.

[^3]: There are
    [multiple kinds](https://devcenter.heroku.com/articles/dynos#dyno-configurations)
    of dynos. However, it's not something that we need to worry about for our
    simple web API.

[^me]: 僕が自分で許可しました。また、翻訳ではなくローカライズなので、原文の逐語訳ではなく、日本語話者にとって理解しやすいように一部加筆修正してあります。

