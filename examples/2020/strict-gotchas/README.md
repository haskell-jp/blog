# haskell-jp-blog-example-strict-gotchas

[こちらのIssue](https://github.com/haskell-jp/blog/issues/167)について、せっかく盛り上がってきたので少しでも進めようと、まずはサンプルを書いてみることにしました。

## 注記

- 記事の内容と大きく連動しそうなので、敢えて同じリポジトリーに上げています。
- 各 `*.hs ` ファイルを `runghc --ghc-arg=-XStrict` で実行した場合と `--ghc-arg=-XStrict` なしで実行した場合とで比較することで、挙動の違いを体験できるように作っています。
