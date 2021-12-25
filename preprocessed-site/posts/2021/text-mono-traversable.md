---
title: 文字列型を抽象化するのにはmono-traversableパッケージがいいかも
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: YAMAMOTO Yuji
postedBy: <a href="http://the.igreque.info/">YAMAMOTO Yuji(@igrep)</a>
date: December 25, 2021
---

この記事は[Haskell Advent Calendar 2021](https://qiita.com/advent-calendar/2021/haskell)の25日目の記事です。

Haskellのよく言われる問題点の一つとして、文字列型が下記のようによく使われるものだけで**5種類**もある、という点があります:

- `String`
- Strictな`Text`
- Lazyな`Text`
- Strictな`ByteString`
- Lazyな`ByteString`

<small>（上記の頻繁に使われるもの以外にも、もっとあります）</small>

それぞれ確かに使いどころが違うので、アプリケーションで使用する場合は場面に応じて使い分ければいいのですが、文字列を使ったライブラリーを開発する場合はなかなか悩ましいものがあります。内部で依存しているライブラリーが使用しているものがあれば、それをそのまま使うのが簡単で確実ですが、そうでない場合も多いでしょう。そこで本稿では文字列型を抽象化して扱いたい場合の手段として、[mono-traversalbeパッケージ](https://hackage.haskell.org/package/mono-traversable)を検討したいと思います。

# mono-traversableパッケージの紹介

mono-traversableパッケージは、名前のとおり`MonoTraversable`や`MonoFoldable`、`MonoFunctor`といったおなじみの型クラスの名前に`Mono`という接頭辞を付けた型クラスによって、多様なコンテナ型を抽象化してくれます。これらの型クラスはすべて、`ByteString`や`Text`のような、「要素として持てる値の型が1種類だけ」の型も対象にしているのが特徴です。Type Familyを応用し、次のように型毎に要素の型を固定することで、そうした特徴を実現しています:

```haskell
type family Element mono

type instance Element ByteString = Word8
type instance Element Text = Char
type instance Element [a] = a

-- ...

class MonoFunctor mono where
  -- ...
  omap :: (Element mono -> Element mono) -> mono -> mono

instance MonoFunctor ByteString where
  omap = ByteString.map

instance MonoFunctor Text where
  omap = Text.map

instance MonoFunctor [a] where
  omap = map
```

※mono-traversableパッケージのソースから引用して少し改変しました。

さらに、これまで紹介した`MonoTraversable`や`MonoFoldable`、`MonoFunctor`に加えて、`SemiSequence`や`IsSequence`という型クラスで分解や構築に関わる操作<small>（例えば`cons`や`break`）</small>などの他、<small>（今回は取り上げませんが）</small>`SetContainer`などの型クラスで`Map`や`Set`、`IntMap`などの型まで抽象化してくれます。

そこで次の節では、このmono-traversableパッケージにおける型クラスを中心に、`Data.Text`モジュールや`Data.ByteString`モジュールにおける各関数が、どの型クラスに対するどの関数に対応するのか、まとめた表を作ってみました。

# mono-traversableパッケージにおける型クラスのメソッドと、各種文字列型向け関数の対応表

- ℹ️調査したmono-traversableパッケージのバージョンは1.0.15.3です
- ℹ️原則として関数の名前しか見ていないので、実際には異なる用途かも知れません
- ℹ️mono-traversableパッケージにある型クラスの他、baseパッケージにある`Monoid`, `Semigroup`などのメソッドも調査対象に含めました
- ℹ️`String`についてはbaseパッケージにある関数のみを対象にしていますが、`Data.List`モジュールのドキュメントと自分の記憶を頼りに埋めているので間違いがあるかも知れません
- ℹ️`Text`・`ByteString`についてはStrictなバージョンのドキュメントのみ参照しています。Lazyな方になかったらごめんなさい！
- ℹ️`Textual`型クラスについては、`ByteString`がインスタンスになっていないのでご注意ください
- ℹ️以下のような関数は除外しました:
    - `IO`が絡むもの
    - プリミティブな処理で使うもの

| `Text`           | `ByteString`     | `String` (`[Char]`) | 型クラス / 関数                 |
| ---------------- | ---------------- | ------------------- | ------------------------------- |
| `all`            | `all`            | `all`               | `MonoFoldable` / `oall`         |
| `any`            | `any`            | `any`               | `MonoFoldable` / `oany`         |
| `append`         | `append`         | `++`                | `Semigroup` / `<>`              |
| N/A              | `breakByte`      | N/A                 | N/A                             |
| N/A              | `breakEnd`       | N/A                 | N/A                             |
| `breakOnAll`     | N/A              | N/A                 | N/A                             |
| `breakOnEnd`     | N/A              | N/A                 | N/A                             |
| `breakOn`        | `breakSubstring` | N/A                 | N/A                             |
| `break`          | `break`          | `break`             | `IsSequence` / `break`          |
| `center`         | N/A              | N/A                 | N/A                             |
| `chunksOf`       | N/A              | N/A                 | N/A                             |
| `commonPrefixes` | N/A              | N/A                 | N/A                             |
| `compareLength`  | N/A              | N/A                 | N/A                             |
| `concatMap`      | N/A              | `concatMap`         | `MonoFoldable` / `ofoldMap`     |
| `concat`         | `concat`         | `concat`            | `MonoFoldable` / `ofold`        |
| `cons`           | `cons`           | N/A                 | `SemiSequence` / `cons`         |
| `copy`           | `copy`           | N/A                 | N/A                             |
| `count`          | `count`          | N/A                 | N/A                             |
| `dropAround`     | N/A              | N/A                 | N/A                             |
| `dropEnd`        | N/A              | N/A                 | `IsSequence` / `dropEnd`        |
| `dropWhileEnd`   | `dropWhileEnd`   | `dropWhileEnd`      | N/A                             |
| `dropWhile`      | `dropWhile`      | `dropWhile`         | `IsSequence` / `dropWhile`      |
| `drop`           | `drop`           | `drop`              | `IsSequence` / `drop`           |
| N/A              | `elemIndexEnd`   | N/A                 | N/A                             |
| N/A              | `elemIndex`      | `elemIndex`         | N/A                             |
| N/A              | `elemIndices`    | `elemIndices`       | N/A                             |
| N/A              | `elem`           | `elem`              | `MonoFoldable` / `oelem`        |
| `empty`          | `empty`          | `""`                | `Monoid` / `mempty`             |
| `filter`         | `filter`         | `filter`            | `IsSequence` / `filter`         |
| N/A              | `findIndexEnd`   | N/A                 | N/A                             |
| `findIndex`      | `findIndex`      | `findIndex`         | N/A                             |
| N/A              | `findIndices`    | `findIndices`       | N/A                             |
| N/A              | `findSubstring`  | N/A                 | N/A                             |
| N/A              | `findSubstrings` | N/A                 | N/A                             |
| `find`           | `find`           | `find`              | `SemiSequence` / `find`         |
| `foldl'`         | `foldl'`         | `foldl'`            | `MonoFoldable` / `ofoldl'`      |
| `foldl1'`        | `foldl1'`        | `foldl1'`           | `MonoFoldable` / `ofoldl1Ex'`   |
| `foldl1`         | `foldl1`         | `foldl1`            | N/A                             |
| `foldl`          | `foldl`          | `foldl`             | N/A                             |
| N/A              | `foldr'`         | N/A                 | N/A                             |
| N/A              | `foldr1'`        | N/A                 | N/A                             |
| `foldr1`         | `foldr1`         | `foldr1`            | `MonoFoldable` / `ofoldr1Ex`    |
| `foldr`          | `foldr`          | `foldr`             | `MonoFoldable` / `ofoldr`       |
| `groupBy`        | `groupBy`        | `groupBy`           | `IsSequence` / `groupBy`        |
| `group`          | `group`          | `group`             | `IsSequence` / `group`          |
| `head`           | `head`           | `head`              | `MonoFoldable` / `headEx`       |
| `index`          | `index`          | `index`             | `IsSequence` / `indexEx`        |
| `init`           | `init`           | `init`              | `IsSequence` / `initEx`         |
| `inits`          | `inits`          | `inits`             | N/A                             |
| `intercalate`    | `intercalate`    | `intercalate`       | `MonoFoldable` / `ointercalate` |
| `intersperse`    | `intersperse`    | `intersperse`       | `SemiSequence` / `intersperse`  |
| `isInfixOf`      | `isInfixOf`      | `isInfixOf`         | `IsSequence` / `isInfixOf`      |
| `isPrefixOf`     | `isPrefixOf`     | `isPrefixOf`        | `IsSequence` / `isPrefixOf`     |
| `isSuffixOf`     | `isSuffixOf`     | `isSuffixOf`        | `IsSequence` / `isSuffixOf`     |
| `justifyLeft`    | N/A              | N/A                 | N/A                             |
| `justifyRight`   | N/A              | N/A                 | N/A                             |
| `last`           | `last`           | `last`              | `MonoFoldable` / `lastEx`       |
| `length`         | `length`         | `length`            | `MonoFoldable` / `olength`      |
| `lines`          | N/A              | `lines`             | `Textual` / `lines`             |
| `mapAccumL`      | `mapAccumL`      | `mapAccumL`         | N/A                             |
| `mapAccumR`      | `mapAccumR`      | `mapAccumR`         | N/A                             |
| `map`            | `map`            | `map`               | `MonoFunctor` / `omap`          |
| `maximum`        | `maximum`        | `maximum`           | `MonoFoldable` / `maximumEx`    |
| `minimum`        | `minimum`        | `minimum`           | `MonoFoldable` / `minimumEx`    |
| N/A              | `notElem`        | `notElem`           | `MonoFoldable` / `onotElem`     |
| `null`           | `null`           | `null`              | `MonoFoldable` / `onull`        |
| `pack`           | `pack`           | `id`                | `IsString` / `fromString`       |
| `partition`      | `partition`      | `partition`         | `IsSequence` / `partition`      |
| `replace`        | N/A              | N/A                 | `IsSequence` / `replaceSeq`     |
| `replicate`      | `replicate`      | `replicate`         | `IsSequence` / `replicate`      |
| `reverse`        | `reverse`        | `reverse`           | `SemiSequence` / `reverse`      |
| `scanl1`         | `scanl1`         | `scanl1`            | N/A                             |
| `scanl`          | `scanl`          | `scanl`             | N/A                             |
| `scanr1`         | `scanr1`         | `scanr1`            | N/A                             |
| `scanr`          | `scanr`          | `scanr`             | N/A                             |
| `singleton`      | `singleton`      | `singleton`         | `MonoPointed` / `opoint`        |
| `snoc`           | `snoc`           | `snoc`              | `SemiSequence` / `snoc`         |
| N/A              | `sort`           | `sort`              | `SemiSequence` / `sort`         |
| N/A              | `spanEnd`        | N/A                 | N/A                             |
| `span`           | `span`           | `span`              | `SemiSequence` / `span`         |
| `splitAt`        | `splitAt`        | `splitAt`           | `IsSequence` / `splitAt`        |
| `splitOn`        | N/A              | `splitOn`           | `IsSequence` / `splitSeq`       |
| N/A              | `splitWith`      | N/A                 | `IsSequence` / `splitElem`      |
| `split`          | `splitWith`      | N/A                 | `IsSequence` / `splitWhen`      |
| `stripEnd`       | N/A              | N/A                 | `N/A`                           |
| `stripPrefix`    | `stripPrefix`    | `stripPrefix`       | `IsSequence` / `stripPrefix`    |
| `stripStart`     | N/A              | N/A                 | N/A                             |
| `stripSuffix`    | `stripSuffix`    | N/A                 | `IsSequence` / `stripSuffix`    |
| `strip`          | N/A              | N/A                 | N/A                             |
| `tail`           | `tail`           | `tail`              | `IsSequence` / `tail`           |
| `tails`          | `tails`          | `tails`             | N/A                             |
| `takeEnd`        | N/A              | N/A                 | N/A                             |
| `takeWhileEnd`   | `takeWhileEnd`   | N/A                 | N/A                             |
| `takeWhile`      | `takeWhile`      | `takeWhile`         | `IsSequence` / `takeWhile`      |
| `take`           | `take`           | `take`              | `IsSequence` / `take`           |
| `toCaseFold`     | N/A              | N/A                 | `Textual` / `toCaseFold`        |
| `toLower`        | N/A              | N/A                 | `Textual` / `toLower`           |
| `toTitle`        | N/A              | N/A                 | N/A                             |
| `toUpper`        | N/A              | N/A                 | `Textual` / `toUpper`           |
| `transpose`      | `transpose`      | N/A                 | N/A                             |
| `uncons`         | `uncons`         | N/A                 | `IsSequence` / `uncons`         |
| `unfoldrN`       | `unfoldrN`       | N/A                 | N/A                             |
| `unfoldr`        | `unfoldr`        | `unfoldr`           | N/A                             |
| `unlines`        | N/A              | `unlines`           | `Textual` / `unlines`           |
| `unpack`         | `unpack`         | `id`                | `MonoFoldable` / `otoList`      |
| `unsnoc`         | `unsnoc`         | `N/A`               | `SemiSequence` / `unsnoc`       |
| `unwords`        | N/A              | `unwords`           | `Textual` / `unwords`           |
| `words`          | N/A              | `words`             | `Textual` / `words`             |
| `zipWith`        | `zipWith`        | `zipWith`           | `MonoZip` / `ozipWith`          |
| `zip`            | `zip`            | `zip`               | `MonoZip` / `ozip`              |

<!--
元ネタとして`:browse`した結果をメモしておきます

> :browse Data.Text
T.all :: (Char -> Bool) -> T.Text -> Bool
T.any :: (Char -> Bool) -> T.Text -> Bool
T.append :: T.Text -> T.Text -> T.Text
T.break :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
T.breakOn :: T.Text -> T.Text -> (T.Text, T.Text)
T.breakOnAll :: T.Text -> T.Text -> [(T.Text, T.Text)]
T.breakOnEnd :: T.Text -> T.Text -> (T.Text, T.Text)
T.center :: Int -> Char -> T.Text -> T.Text
T.chunksOf :: Int -> T.Text -> [T.Text]
T.commonPrefixes ::
  T.Text -> T.Text -> Maybe (T.Text, T.Text, T.Text)
T.compareLength :: T.Text -> Int -> Ordering
T.concat :: [T.Text] -> T.Text
T.concatMap :: (Char -> T.Text) -> T.Text -> T.Text
T.cons :: Char -> T.Text -> T.Text
T.copy :: T.Text -> T.Text
T.count :: T.Text -> T.Text -> Int
T.drop :: Int -> T.Text -> T.Text
T.dropAround :: (Char -> Bool) -> T.Text -> T.Text
T.dropEnd :: Int -> T.Text -> T.Text
T.dropWhile :: (Char -> Bool) -> T.Text -> T.Text
T.dropWhileEnd :: (Char -> Bool) -> T.Text -> T.Text
T.filter :: (Char -> Bool) -> T.Text -> T.Text
T.find :: (Char -> Bool) -> T.Text -> Maybe Char
T.findIndex :: (Char -> Bool) -> T.Text -> Maybe Int
T.foldl :: (a -> Char -> a) -> a -> T.Text -> a
T.foldl' :: (a -> Char -> a) -> a -> T.Text -> a
T.foldl1 :: (Char -> Char -> Char) -> T.Text -> Char
T.foldl1' :: (Char -> Char -> Char) -> T.Text -> Char
T.foldr :: (Char -> a -> a) -> a -> T.Text -> a
T.foldr1 :: (Char -> Char -> Char) -> T.Text -> Char
T.group :: T.Text -> [T.Text]
T.groupBy :: (Char -> Char -> Bool) -> T.Text -> [T.Text]
T.head :: T.Text -> Char
T.index :: T.Text -> Int -> Char
T.init :: T.Text -> T.Text
T.inits :: T.Text -> [T.Text]
T.intercalate :: T.Text -> [T.Text] -> T.Text
T.intersperse :: Char -> T.Text -> T.Text
T.isInfixOf :: T.Text -> T.Text -> Bool
T.isPrefixOf :: T.Text -> T.Text -> Bool
T.isSuffixOf :: T.Text -> T.Text -> Bool
T.justifyLeft :: Int -> Char -> T.Text -> T.Text
T.justifyRight :: Int -> Char -> T.Text -> T.Text
T.last :: T.Text -> Char
T.length :: T.Text -> Int
T.lines :: T.Text -> [T.Text]
T.map :: (Char -> Char) -> T.Text -> T.Text
T.mapAccumL ::
  (a -> Char -> (a, Char)) -> a -> T.Text -> (a, T.Text)
T.mapAccumR ::
  (a -> Char -> (a, Char)) -> a -> T.Text -> (a, T.Text)
T.maximum :: T.Text -> Char
T.minimum :: T.Text -> Char
T.null :: T.Text -> Bool
T.pack :: String -> T.Text
T.partition :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
T.replace :: T.Text -> T.Text -> T.Text -> T.Text
T.replicate :: Int -> T.Text -> T.Text
T.reverse :: T.Text -> T.Text
T.scanl :: (Char -> Char -> Char) -> Char -> T.Text -> T.Text
T.scanl1 :: (Char -> Char -> Char) -> T.Text -> T.Text
T.scanr :: (Char -> Char -> Char) -> Char -> T.Text -> T.Text
T.scanr1 :: (Char -> Char -> Char) -> T.Text -> T.Text
T.snoc :: T.Text -> Char -> T.Text
T.span :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
T.split :: (Char -> Bool) -> T.Text -> [T.Text]
T.splitAt :: Int -> T.Text -> (T.Text, T.Text)
T.splitOn :: T.Text -> T.Text -> [T.Text]
T.strip :: T.Text -> T.Text
T.stripEnd :: T.Text -> T.Text
T.stripPrefix :: T.Text -> T.Text -> Maybe T.Text
T.stripStart :: T.Text -> T.Text
T.stripSuffix :: T.Text -> T.Text -> Maybe T.Text
T.tail :: T.Text -> T.Text
T.tails :: T.Text -> [T.Text]
T.take :: Int -> T.Text -> T.Text
T.takeEnd :: Int -> T.Text -> T.Text
T.takeWhile :: (Char -> Bool) -> T.Text -> T.Text
T.takeWhileEnd :: (Char -> Bool) -> T.Text -> T.Text
T.toCaseFold :: T.Text -> T.Text
T.toLower :: T.Text -> T.Text
T.toTitle :: T.Text -> T.Text
T.toUpper :: T.Text -> T.Text
T.transpose :: [T.Text] -> [T.Text]
T.uncons :: T.Text -> Maybe (Char, T.Text)
T.unfoldr :: (a -> Maybe (Char, a)) -> a -> T.Text
T.unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> T.Text
T.unlines :: [T.Text] -> T.Text
T.unsnoc :: T.Text -> Maybe (T.Text, Char)
T.unwords :: [T.Text] -> T.Text
T.words :: T.Text -> [T.Text]
T.zip :: T.Text -> T.Text -> [(Char, Char)]
T.zipWith :: (Char -> Char -> Char) -> T.Text -> T.Text -> T.Text
type T.Text :: *
data T.Text
  = Data.Text.Internal.Text {-# UNPACK #-}Data.Text.Array.Array
                            {-# UNPACK #-}Int
                            {-# UNPACK #-}Int
T.empty :: T.Text
T.singleton :: Char -> T.Text
T.unpack :: T.Text -> String
T.unpackCString# :: GHC.Prim.Addr# -> T.Text

B.all :: (GHC.Word.Word8 -> Bool) -> B.ByteString -> Bool
B.any :: (GHC.Word.Word8 -> Bool) -> B.ByteString -> Bool
B.append :: B.ByteString -> B.ByteString -> B.ByteString
B.appendFile :: FilePath -> B.ByteString -> IO ()
B.break ::
  (GHC.Word.Word8 -> Bool)
  -> B.ByteString -> (B.ByteString, B.ByteString)
B.breakByte ::
  GHC.Word.Word8 -> B.ByteString -> (B.ByteString, B.ByteString)
B.breakEnd ::
  (GHC.Word.Word8 -> Bool)
  -> B.ByteString -> (B.ByteString, B.ByteString)
B.breakSubstring ::
  B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
B.concat :: [B.ByteString] -> B.ByteString
B.concatMap ::
  (GHC.Word.Word8 -> B.ByteString) -> B.ByteString -> B.ByteString
B.cons :: GHC.Word.Word8 -> B.ByteString -> B.ByteString
B.copy :: B.ByteString -> B.ByteString
B.count :: GHC.Word.Word8 -> B.ByteString -> Int
B.drop :: Int -> B.ByteString -> B.ByteString
B.dropWhile ::
  (GHC.Word.Word8 -> Bool) -> B.ByteString -> B.ByteString
B.dropWhileEnd ::
  (GHC.Word.Word8 -> Bool) -> B.ByteString -> B.ByteString
B.elem :: GHC.Word.Word8 -> B.ByteString -> Bool
B.elemIndex :: GHC.Word.Word8 -> B.ByteString -> Maybe Int
B.elemIndexEnd :: GHC.Word.Word8 -> B.ByteString -> Maybe Int
B.elemIndices :: GHC.Word.Word8 -> B.ByteString -> [Int]
B.empty :: B.ByteString
B.filter ::
  (GHC.Word.Word8 -> Bool) -> B.ByteString -> B.ByteString
B.find ::
  (GHC.Word.Word8 -> Bool) -> B.ByteString -> Maybe GHC.Word.Word8
B.findIndex ::
  (GHC.Word.Word8 -> Bool) -> B.ByteString -> Maybe Int
B.findIndexEnd ::
  (GHC.Word.Word8 -> Bool) -> B.ByteString -> Maybe Int
B.findIndices :: (GHC.Word.Word8 -> Bool) -> B.ByteString -> [Int]
B.findSubstring :: B.ByteString -> B.ByteString -> Maybe Int
B.findSubstrings :: B.ByteString -> B.ByteString -> [Int]
B.foldl :: (a -> GHC.Word.Word8 -> a) -> a -> B.ByteString -> a
B.foldl' :: (a -> GHC.Word.Word8 -> a) -> a -> B.ByteString -> a
B.foldl1 ::
  (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8)
  -> B.ByteString -> GHC.Word.Word8
B.foldl1' ::
  (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8)
  -> B.ByteString -> GHC.Word.Word8
B.foldr :: (GHC.Word.Word8 -> a -> a) -> a -> B.ByteString -> a
B.foldr' :: (GHC.Word.Word8 -> a -> a) -> a -> B.ByteString -> a
B.foldr1 ::
  (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8)
  -> B.ByteString -> GHC.Word.Word8
B.foldr1' ::
  (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8)
  -> B.ByteString -> GHC.Word.Word8
B.getContents :: IO B.ByteString
B.getLine :: IO B.ByteString
B.group :: B.ByteString -> [B.ByteString]
B.groupBy ::
  (GHC.Word.Word8 -> GHC.Word.Word8 -> Bool)
  -> B.ByteString -> [B.ByteString]
B.hGet :: GHC.IO.Handle.Types.Handle -> Int -> IO B.ByteString
B.hGetContents :: GHC.IO.Handle.Types.Handle -> IO B.ByteString
B.hGetLine :: GHC.IO.Handle.Types.Handle -> IO B.ByteString
B.hGetNonBlocking ::
  GHC.IO.Handle.Types.Handle -> Int -> IO B.ByteString
B.hGetSome :: GHC.IO.Handle.Types.Handle -> Int -> IO B.ByteString
B.hPut :: GHC.IO.Handle.Types.Handle -> B.ByteString -> IO ()
B.hPutNonBlocking ::
  GHC.IO.Handle.Types.Handle -> B.ByteString -> IO B.ByteString
B.hPutStr :: GHC.IO.Handle.Types.Handle -> B.ByteString -> IO ()
B.hPutStrLn :: GHC.IO.Handle.Types.Handle -> B.ByteString -> IO ()
B.head :: B.ByteString -> GHC.Word.Word8
B.index :: B.ByteString -> Int -> GHC.Word.Word8
B.init :: B.ByteString -> B.ByteString
B.inits :: B.ByteString -> [B.ByteString]
B.interact :: (B.ByteString -> B.ByteString) -> IO ()
B.intercalate :: B.ByteString -> [B.ByteString] -> B.ByteString
B.intersperse :: GHC.Word.Word8 -> B.ByteString -> B.ByteString
B.isInfixOf :: B.ByteString -> B.ByteString -> Bool
B.isPrefixOf :: B.ByteString -> B.ByteString -> Bool
B.isSuffixOf :: B.ByteString -> B.ByteString -> Bool
B.last :: B.ByteString -> GHC.Word.Word8
B.length :: B.ByteString -> Int
B.map ::
  (GHC.Word.Word8 -> GHC.Word.Word8) -> B.ByteString -> B.ByteString
B.mapAccumL ::
  (acc -> GHC.Word.Word8 -> (acc, GHC.Word.Word8))
  -> acc -> B.ByteString -> (acc, B.ByteString)
B.mapAccumR ::
  (acc -> GHC.Word.Word8 -> (acc, GHC.Word.Word8))
  -> acc -> B.ByteString -> (acc, B.ByteString)
B.maximum :: B.ByteString -> GHC.Word.Word8
B.minimum :: B.ByteString -> GHC.Word.Word8
B.notElem :: GHC.Word.Word8 -> B.ByteString -> Bool
B.null :: B.ByteString -> Bool
B.pack :: [GHC.Word.Word8] -> B.ByteString
B.packCString :: Foreign.C.String.CString -> IO B.ByteString
B.packCStringLen :: Foreign.C.String.CStringLen -> IO B.ByteString
B.partition ::
  (GHC.Word.Word8 -> Bool)
  -> B.ByteString -> (B.ByteString, B.ByteString)
B.putStr :: B.ByteString -> IO ()
B.putStrLn :: B.ByteString -> IO ()
B.readFile :: FilePath -> IO B.ByteString
B.replicate :: Int -> GHC.Word.Word8 -> B.ByteString
B.reverse :: B.ByteString -> B.ByteString
B.scanl ::
  (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8)
  -> GHC.Word.Word8 -> B.ByteString -> B.ByteString
B.scanl1 ::
  (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8)
  -> B.ByteString -> B.ByteString
B.scanr ::
  (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8)
  -> GHC.Word.Word8 -> B.ByteString -> B.ByteString
B.scanr1 ::
  (GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8)
  -> B.ByteString -> B.ByteString
B.singleton :: GHC.Word.Word8 -> B.ByteString
B.snoc :: B.ByteString -> GHC.Word.Word8 -> B.ByteString
B.sort :: B.ByteString -> B.ByteString
B.span ::
  (GHC.Word.Word8 -> Bool)
  -> B.ByteString -> (B.ByteString, B.ByteString)
B.spanEnd ::
  (GHC.Word.Word8 -> Bool)
  -> B.ByteString -> (B.ByteString, B.ByteString)
B.split :: GHC.Word.Word8 -> B.ByteString -> [B.ByteString]
B.splitAt :: Int -> B.ByteString -> (B.ByteString, B.ByteString)
B.splitWith ::
  (GHC.Word.Word8 -> Bool) -> B.ByteString -> [B.ByteString]
B.stripPrefix :: B.ByteString -> B.ByteString -> Maybe B.ByteString
B.stripSuffix :: B.ByteString -> B.ByteString -> Maybe B.ByteString
B.tail :: B.ByteString -> B.ByteString
B.tails :: B.ByteString -> [B.ByteString]
B.take :: Int -> B.ByteString -> B.ByteString
B.takeWhile ::
  (GHC.Word.Word8 -> Bool) -> B.ByteString -> B.ByteString
B.takeWhileEnd ::
  (GHC.Word.Word8 -> Bool) -> B.ByteString -> B.ByteString
B.transpose :: [B.ByteString] -> [B.ByteString]
B.uncons :: B.ByteString -> Maybe (GHC.Word.Word8, B.ByteString)
B.unfoldr :: (a -> Maybe (GHC.Word.Word8, a)) -> a -> B.ByteString
B.unfoldrN ::
  Int
  -> (a -> Maybe (GHC.Word.Word8, a)) -> a -> (B.ByteString, Maybe a)
B.unpack :: B.ByteString -> [GHC.Word.Word8]
B.unsnoc :: B.ByteString -> Maybe (B.ByteString, GHC.Word.Word8)
B.unzip ::
  [(GHC.Word.Word8, GHC.Word.Word8)] -> (B.ByteString, B.ByteString)
B.useAsCString ::
  B.ByteString -> (Foreign.C.String.CString -> IO a) -> IO a
B.useAsCStringLen ::
  B.ByteString -> (Foreign.C.String.CStringLen -> IO a) -> IO a
B.writeFile :: FilePath -> B.ByteString -> IO ()
B.zip ::
  B.ByteString -> B.ByteString -> [(GHC.Word.Word8, GHC.Word.Word8)]
B.zipWith ::
  (GHC.Word.Word8 -> GHC.Word.Word8 -> a)
  -> B.ByteString -> B.ByteString -> [a]
type B.ByteString :: *
data B.ByteString
  = Data.ByteString.Internal.PS {-# UNPACK #-}(GHC.ForeignPtr.ForeignPtr
                                                 GHC.Word.Word8)
                                {-# UNPACK #-}Int
                                {-# UNPACK #-}Int
-->

以上です。残念ながら万能とはいかないようで、いくつか「N/A」、すなわち対応するものがない関数もありますが、他の関数の組み合わせで実装できるものもあるでしょう。

# ⚠️パフォーマンスに関わる注意事項

`MonoTraversable`などに限らず、型クラスを使って関数を多相化したとき全般に言えることですが、コンパイル時にインスタンスの解決が行えなかった場合、直接対象の型の相当する関数を呼ぶより少し遅くなってしまう場合があります（[参考](https://blog.miz-ar.info/2016/06/writing-efficient-program-with-haskell/#2.specialization)）。

また、それに限らず、各型クラスのメソッドでない関数は、各型の相当する関数でオーバーライドできないため、効率の悪い処理になってしまう恐れがあります。例えば、[`ointercalate`関数](https://hackage.haskell.org/package/mono-traversable-1.0.15.3/docs/src/Data.MonoTraversable.html#ointercalate)の実装を見ると、`Text`や`ByteString`などについては`RULES`プラグマで最適な実装を設定しているようですが、それ以外の型については一旦リストに変換してから結合する、という効率の悪そうな処理をしています。

# 事例: `String`から相互変換できる型を抽象化する

最後に、最近私が作った（まだリリースしてない）ライブラリーにおいて、`MonoFoldable`と`IsString`を使うことで、`Text`と`String`両方をサポートした関数を紹介しておきます。ただ、時間とやる気パワーが残り少なくなってしまったので、該当の箇所だけ[こちら](https://github.com/igrep/envparse-applicative/blob/0fb7b23e45a09b4f53406b46bd563312ed27f2a4/src/EnvParse/Applicative.hs#L156)からコピペして、説明は簡単にしておきます:

```haskell
stringVal :: (IsString a, MT.MonoFoldable a, MT.Element a ~ Char) => CodecEnvVal a
stringVal = valByFunction CodecEnvValByFunction
  { encode = MT.otoList
  , decode = Right . fromString
  }
```

`CodecEnvVal a`型は、`a`型を`String`型と相互変換するための情報を含んだ型です。`stringVal`の場合、名前のとおり文字列っぽい型と`String`との相互変換ができなければなりません。もちろん単純に`String`型だけをサポートして`Text`用には別途`CodecEnvVal Text`を作ってもいいのですが、一つの`CodecEnvVal a`だけで扱えた方が楽でしょうし、今回は`MonoFoldable`の`otoList`と`IsString`の`fromString`を使って両方をサポートすることにしました。なお、これでは`ByteString`がサポートできませんが、ここで相互変換する`String`は、要件上人間が読み書きするファイルにおける文字列を想定しているので、`ByteString`はバイナリーデータにだけ使うべきだ、という立場から敢えてサポートしていません。

# まとめ

mono-traversableパッケージをうまく使えば、自前で専用の型クラスを作らなくても`String`・`Text`・`ByteString`などを一挙にサポートする関数が書けるかも知れません！

それでは2022年はmono-traversableでHappy Haskell String Programming!🚝
