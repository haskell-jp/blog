{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative (empty)
import Control.Monad (filterM)
import Data.Char (isLatin1)
import Data.Data (Data)
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty((:|)), groupBy, toList)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Tree as Tree
import Data.Typeable (Typeable)
import Hakyll
import Skylighting (pygments, styleToCss)
import Text.Pandoc.Definition ( Inline(Space, Span, Str)
                              , Pandoc(Pandoc)
                              , Block(Header)
                              , Inline(Link)
                              , Attr
                              )
import qualified Text.Pandoc.Builder as PB
import Text.Pandoc.Generic (bottomUp)

-- | Change some of the default configuration variables.  This makes our
-- project working directory a little cleaner.
hakyllConfig :: Configuration
hakyllConfig = def { providerDirectory = "preprocessed-site"
                   , storeDirectory = ".hakyll-cache"
                   , tmpDirectory = ".hakyll-cache/tmp"
                   , destinationDirectory = "generated-site"
                   }

main :: IO ()
main = hakyllWith hakyllConfig $ do

    -- templates for other routes
    match "templates/*" $ compile templateCompiler

    -- images
    match "img/**" $ do
        route idRoute
        compile copyFileCompiler

    -- CSS
    create ["css/syntax.css"] $ do
      route idRoute
      compile $ makeItem (compressCss $ styleToCss pygments)

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Javascript
    match "js/*" $ do
        route idRoute
        compile copyFileCompiler

    -- web fonts
    match "fonts/*" $ do
        route idRoute
        compile copyFileCompiler

    -- index.html
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/**"
            -- Instead of just (return posts) here, I might want to take
            -- each post Item and add a field about whether or not it is
            -- the last Item.  Then, in templates/post-list.html, I can
            -- look at whether or not it is last, and only add a <hr> if it
            -- is not last.  Also, it should be ignored if the "draft"
            -- metadata is set, because it won't be shown.
            --
            -- The following post might help?
            -- https://github.com/jaspervdj/hakyll/issues/263
            let indexCtx = listField "posts" postCtx (return posts) `mappend`
                           defaultContext
            indexBody <- getResourceBody
            indexWithContext <- applyAsTemplate indexCtx indexBody
            applyDefaultTemplate indexCtx indexWithContext

    -- blog posts
    match "posts/**" $ do
        route postsAndDraftsRoutes
        compile $ do
            let relatedPostCtx = prevPostCtx `mappend` nextPostCtx
                lookupUrl ident = maybe empty (pure . toUrl) =<< getRoute ident
                lookupTitle ident = maybe empty pure =<< getMetadataField ident "title"
                lookupLanguage ident = fromMaybe "ja" <$> getMetadataField ident "language"
                prevPostCtx = field "prevPostUrl" (getPrevPostField lookupUrl) `mappend`
                              field "prevPostTitle" (getPrevPostField lookupTitle) `mappend`
                              field "prevPostLanguage" (getPrevPostField lookupLanguage)
                nextPostCtx = field "nextPostUrl" (getNextPostField lookupUrl) `mappend`
                              field "nextPostTitle" (getNextPostField lookupTitle)`mappend`
                              field "nextPostLanguage" (getNextPostField lookupLanguage)
            let postsCtx =
                    field "description" createOpenGraphDescription `mappend`
                    boolField "article" (const True) `mappend`
                    field "subHeadingContent" createSubHeadingContentForPost `mappend`
                    relatedPostCtx `mappend`
                    postCtx
            language <-
              fmap (fromMaybe "ja") $ (`getMetadataField` "language") =<< getUnderlying
            pandocOut <-
                pandocCompilerWithTransform
                  defaultHakyllReaderOptions
                  defaultHakyllWriterOptions
                  (postMakeTOC . addSpaceAroundAsciiPandoc language)
            postTemplateOut <- loadAndApplyTemplate postTemplate postsCtx pandocOut
            applyDefaultTemplate postsCtx =<< saveSnapshot "content" postTemplateOut

    -- atom feed
    feedConfig <- getFeedConfig "feed.md"
    create ["feed.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/**" "content"
            renderAtom feedConfig feedCtx posts

-- | For posts, add a @date@ field to the default context.
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend`
          dateField "year" "%Y" `mappend`
          defaultContext

-- | Apply the default template and then relativize all the URLs in the
-- resulting html file.
applyDefaultTemplate :: Context a -> Item a -> Compiler (Item String)
applyDefaultTemplate context preTemplateItem = do
    postTemplateItem <- loadAndApplyTemplate defaultTemplate context preTemplateItem
    relativizeUrls postTemplateItem

-- | This is our default site template that all html files should go
-- through.  This default template defines our site HTML \<head\> tag.
defaultTemplate :: Identifier
defaultTemplate = "templates/default.html"

-- | This is the default template for posts.  It basically just defines an
-- \<article\> wrapper for the post body.
postTemplate :: Identifier
postTemplate = "templates/post.html"

-- | Create the description for open graph protocol using body string.
createOpenGraphDescription :: Item a -> Compiler String
createOpenGraphDescription _ = convert . itemBody <$> getResourceBody
  where
    convert = take 200 . escapeHtml . concat . lines

-- | Create the HTML tags for the subheading and "Posted by" lines for
-- a blog post.
--
-- This is kind of hacky.
createSubHeadingContentForPost :: Item a -> Compiler String
createSubHeadingContentForPost item = do
    let ident = itemIdentifier item
    subHeading    <- fromMaybe "" <$> getMetadataField ident "subHeading"
    author        <- getMetadataField' ident "author"
    maybePostedBy <- getMetadataField ident "postedBy"
    date          <- getMetadataField' ident "date"
    mtags         <- getMetadataField ident "tags"
    let subHeadingHtml = "<h2 class=\"subheading\">" ++ subHeading ++ "</h2>"
        postedBy = fromMaybe author maybePostedBy
        postedByHtml = "<span class=\"meta\">Posted by " ++ postedBy ++ " on " ++ date ++  "</span>"
        tagsHtml = maybe "" (\tags -> "<span class=\"meta\">Tags: " ++ tags ++ "</span>") mtags
    return $ subHeadingHtml ++ postedByHtml ++ tagsHtml

isDraftPost :: MonadMetadata m => Identifier -> m Bool
isDraftPost ident = do
    isDraft <- fromMaybe "false" <$> getMetadataField ident "draft"
    return $ isDraft == "true"

postItemIdentifiers :: MonadMetadata m => m [Identifier]
postItemIdentifiers = do
    idents <- getMatches "posts/**"
    idents' <- filterM (fmap not . isDraftPost) idents
    sortRecentFirst idents'

lookupPostField :: ([Identifier] -> Maybe Identifier) -> (Identifier -> Compiler a) -> Compiler a
lookupPostField lookupIdent lookupField = do
    itemIdents <- postItemIdentifiers
    let mtargetIdent = lookupIdent itemIdents

    maybe empty lookupField mtargetIdent

getPrevPostField :: (Identifier -> Compiler b) -> Item a -> Compiler b
getPrevPostField lookupField item = do
    let ident = itemIdentifier item
    lookupPostField (\itemIdents -> lookup ident $ zip itemIdents (tail itemIdents)) lookupField

getNextPostField :: (Identifier -> Compiler b) -> Item a -> Compiler b
getNextPostField lookupField item = do
    let ident = itemIdentifier item
    lookupPostField (\itemIdents -> lookup ident $ zip (tail itemIdents) itemIdents) lookupField

-- | If posts have a "draft" metadata, then this changes their output
-- location from "posts/" to "drafts/".
postsAndDraftsRoutes :: Routes
postsAndDraftsRoutes = metadataRoute $ \metadata ->
    case lookupString "draft" metadata of
        Just _ ->
            gsubRoute "posts/" (const "drafts/") `composeRoutes`
            setExtension "html"
        Nothing -> setExtension "html"

-- | All blocks of Latin text will be placed in @\<span\>@ tags with an
-- @\"ascii\"@ class.
addSpaceAroundAsciiPandoc
  :: String
  -- ^ Language of the article got from the language metadata.
  --   This transformation should be executed only for Japanese articles.
  --   Related: https://github.com/haskell-jp/blog/issues/119 https://github.com/haskell-jp/blog/pull/121
  -> Pandoc -- ^ Content of the article before inserting the span tags.
  -> Pandoc
addSpaceAroundAsciiPandoc lang =
  if lang == "ja"
    then bottomUp (collapseAsciiSpan . addSpaceAroundAsciiInlines)
    else id

collapseAsciiSpan :: [Inline] -> [Inline]
collapseAsciiSpan (AsciiSpan innerInlinesA : Space : AsciiSpan innerInlinesB : other) =
  let newInnerSpan = innerInlinesA ++ [Space] ++ innerInlinesB
  in collapseAsciiSpan $ Span ("", ["ascii"], []) newInnerSpan : other
collapseAsciiSpan (inline : inlines) = inline : collapseAsciiSpan inlines
collapseAsciiSpan [] = []

pattern AsciiSpan :: [Inline] -> Inline
pattern AsciiSpan innerInlines <- Span ("", ["ascii"], []) innerInlines

addSpaceAroundAsciiInlines :: [Inline] -> [Inline]
addSpaceAroundAsciiInlines = concatMap addSpaceInline

-- | Convert the 'String' in a 'Str' to a list of 'TextLang' using
-- 'splitOnLanguage'.  Turn those 'TextLang' into 'Inline' using
-- 'textLangToInline'.  Blocks of Latin text will be placed in @\<span\>@ tags
-- with an @\"ascii\"@ class.
addSpaceInline :: Inline -> [Inline]
addSpaceInline (Str string) = textLangToInline <$> splitOnLanguage string
addSpaceInline inline = [inline]

-- | This is a tag around a 'String' representing whether the 'String' is Latin
-- ('English') or 'Japanese'.  'String's that are neither Latin or 'Japanese'
-- are treated as 'Japanese'.  See 'charLangToTextLang'.
data TextLang
  = Japanese String
  | English String
  deriving (Data, Eq, Read, Show, Typeable)

-- | This is a tag around a 'Char' representing whether the 'Char' is Latin or
-- Japanese.  Characters that are neither Latin or Japanese are treated as
-- Japanese.  See 'charToCharLang'.
data CharLang
  = JapaneseChar Char
  | EnglishChar Char
  deriving (Data, Eq, Read, Show, Typeable)

-- | Convert a 'TextLang' to an 'Inline'.  'Japanese' will be converted to a
-- simple 'Str', while 'English' will be converted to a 'Span' with a
-- @\"ascii\"@ attribute.
--
-- Japanese:
--
-- >>> textLangToInline $ Japanese "日本語"
-- Str "\26085\26412\35486"
--
-- English:
--
-- >>> textLangToInline $ English "foobar"
-- Span ("",["ascii"],[]) [Str "foobar"]
textLangToInline :: TextLang -> Inline
textLangToInline (Japanese string) = Str string
textLangToInline (English string) = Span ("", ["ascii"], []) [Str string]

-- | Split a 'String' into groups of 'TextLang'.
--
--
-- Mix of Japanese and English:
--
-- >>> splitOnLanguage "今日Haskellを紹介"
-- [Japanese "\20170\26085",English "Haskell",Japanese "\12434\32057\20171"]
--
-- Just English:
--
-- >>> splitOnLanguage "This is only English"
-- [English "This is only English"]
--
-- Just Japanese:
--
-- >>> splitOnLanguage "日本語"
-- [Japanese "\26085\26412\35486"]
splitOnLanguage :: String -> [TextLang]
splitOnLanguage [] = []
splitOnLanguage (c:cs) =
  fmap charLangToTextLang . groupByCharLang $ charToCharLang <$> (c :| cs)

-- | Convert all Latin characters to 'EnglishChar' and all other characters to
-- 'JapaneseChar'.
--
-- >>> charToCharLang 'x'
-- EnglishChar 'x'
-- >>> charToCharLang '本'
-- JapaneseChar '\26412'
charToCharLang :: Char -> CharLang
charToCharLang c
  | isLatin1 c = EnglishChar c
  | otherwise = JapaneseChar c

-- | Pull out the 'Char' from 'CharLang'.
--
-- >>> charLangToChar $ EnglishChar 'x'
-- 'x'
-- >>> charLangToChar $ JapaneseChar '本'
-- '\26412'
charLangToChar :: CharLang -> Char
charLangToChar (JapaneseChar c) = c
charLangToChar (EnglishChar c) = c

-- | Group lists of 'CharLang' based on their language.
--
-- Test setup functions:
--
-- >>> toCharLang = fmap charToCharLang
-- >>> fromCharLang = fmap (toList . fmap charLangToChar)
--
-- Mix of Japanese and English:
--
-- >>> let chars = 'f' :| "ooほげほげbar"
-- >>> fromCharLang . groupByCharLang $ toCharLang chars
-- ["foo","\12411\12370\12411\12370","bar"]
--
-- Just Japanese:
--
-- >>> let chars = 'ヤ' :| "ギ"
-- >>> fromCharLang . groupByCharLang $ toCharLang chars
-- ["\12516\12462"]
--
-- Just English:
--
-- >>> let chars = 'g' :| "oat"
-- >>> fromCharLang . groupByCharLang $ toCharLang chars
-- ["goat"]
groupByCharLang :: NonEmpty CharLang -> [NonEmpty CharLang]
groupByCharLang = groupBy f
  where
    f :: CharLang -> CharLang -> Bool
    f JapaneseChar{} JapaneseChar{} = True
    f EnglishChar{} EnglishChar{} = True
    f _ _ = False

-- | Convert groups of 'CharLang' into a 'TextLang'.  This determines the
-- 'TextLang' by looking at the very first character in the group of
-- 'CharLang'.
--
-- This function doesn't handle groups of 'CharLang' that are in a different
-- language.
--
-- Group of 'EnglishChar' gets mapped to 'English':
--
-- >>> charLangToTextLang $ charToCharLang <$> 'f' :| "oobar"
-- English "foobar"
--
-- Group of 'JapaneseChar' gets mapped to 'Japanese':
--
-- >>> charLangToTextLang $ charToCharLang <$> '交' :| "番"
-- Japanese "\20132\30058"
charLangToTextLang :: NonEmpty CharLang -> TextLang
charLangToTextLang cs@(JapaneseChar{} :| _) =
  Japanese . toList $ charLangToChar <$> cs
charLangToTextLang cs@(EnglishChar{} :| _) =
  English . toList $ charLangToChar <$> cs

getFeedConfig :: MonadMetadata m => Identifier -> m FeedConfiguration
getFeedConfig ident = do
    feedTitle       <- getMetadataField' ident "title"
    feedDescription <- getMetadataField' ident "description"
    feedAuthorName  <- getMetadataField' ident "author"
    feedAuthorEmail <- getMetadataField' ident "email"
    feedRoot        <- getMetadataField' ident "root"
    pure FeedConfiguration{..}

-- | Intermediate data that contains a section of a post.
data PostSection = PostSection
  {
    psDepth :: Int,
    psHeaderAttr :: Attr,
    psHeaderInline :: [Inline],
    psContent :: [Block]
  } deriving Show

-- | Extract element ID from `Attr`.
attrId :: Attr -> String
attrId (x, _, _) = x

-- | Construct `Attr` with ID.
idAttr :: String -> Attr
idAttr s = (s, [], [])

-- | Construct `Attr` with one class.
classAttr :: String -> Attr
classAttr s = ("", [s], [])

-- | Eliminate all links from `Block`s or `Inline`s.
elimLink :: Data a => a -> a
elimLink = bottomUp $ foldMap go
  where
    go :: Inline -> [Inline]
    go (Link _ cnt _) = cnt
    go x              = [x]

-- | Pick up all header tag from a document and split it into sections.
-- the 1st element of the result is the leading(non-headered) part of the post.
splitIntoSections :: [Block] -> ([Block], [PostSection])
splitIntoSections = foldr spl ([], [])
  where
    spl (Header d a i) ~(leading, secs) = ([], PostSection d a i leading : secs)
    spl l              ~(leading, secs) = (l : leading, secs)

-- | Depth-first construction of trees.
--
-- *  If the 1st element of the result is 1-element list,
--    it adds a sibling node of the current tip like `unfoldr`.
-- *  If one result containes more than two elements, it increases the depth and move the tip on it.
-- *  A null result finishes the current tip and decreases the depth.
unfoldTree_DF :: (b -> ([a], b)) -> b -> Tree.Forest a
unfoldTree_DF f x0 = fst $ go (f x0)
  where
    go ([], b) = ([], b)
    go (x:xs, b) =
        let (ch, b')    = go (xs, b)
            (sibs, b'') = go (f b')
          in  (Tree.Node x ch : sibs, b'')

-- | Apply a function unless the argument is null.
exceptNull :: (Foldable f, Monoid b) => (f a -> b) -> f a -> b
exceptNull f x | null x    = mempty
               | otherwise = f x

-- | Generate the list part of the table of contents.
makeTOCList :: [PostSection] -> PB.Blocks
makeTOCList [] = mempty
makeTOCList xs0@(x0 : _) = PB.bulletList $ Tree.foldTree fld <$> unfoldTree_DF unf (psDepth x0, xs0)
  where
    unf (n, []) = ([], (n, []))
    unf (n, x:xs)
      | psDepth x < n  =                                        ([],       (n-1, x:xs))
      | psDepth x == n = let (r, (n', ys)) = unf (n+1, xs)   in (Just x : r,  (n', ys))
      | otherwise      = let (r, (n', ys)) = unf (n+1, x:xs) in (Nothing : r, (n', ys))

    fld hd ch = foldMap listItem hd <> exceptNull PB.bulletList ch

    listItem :: PostSection -> PB.Blocks
    listItem PostSection{..} =
        PB.plain $ PB.link ("#" ++ attrId psHeaderAttr)
                           (attrId psHeaderAttr)
                           (PB.fromList $ elimLink psHeaderInline)

-- | Generate each section block with headings.
makeSectionBlock :: PostSection -> PB.Blocks
makeSectionBlock PostSection{..} = hd <> PB.fromList psContent
  where
    hd = PB.headerWith psHeaderAttr psDepth $
           anchor
           <> PB.fromList psHeaderInline

    anchor = PB.spanWith (classAttr "link-to-here-outer") $
               PB.link ("#" ++ attrId psHeaderAttr)
                       (attrId psHeaderAttr) $
                 PB.spanWith (classAttr "link-to-here") $
                   PB.str "Link to" <> PB.linebreak <> PB.str "here"

-- | Transformation function that generates the Table of Contents,
-- "link to here" buttons.
postMakeTOC :: Pandoc -> Pandoc
postMakeTOC (Pandoc meta blk0) = Pandoc meta (PB.toList processed)
  where
    (leading, sections) = splitIntoSections blk0

    processed = PB.fromList leading
                <> toc
                <> foldMap makeSectionBlock sections

    toc :: PB.Blocks
    toc = exceptNull `flip` makeTOCList sections $ \tocList ->
            PB.divWith (idAttr "table-of-contents-outer") $
              PB.divWith (idAttr "table-of-contents") $
                PB.divWith (classAttr "table-of-contents-title")
                  (PB.plain $ PB.str "Contents")
                <> tocList

