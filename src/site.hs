{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Char (isLatin1)
import Data.Data (Data)
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty((:|)), groupBy, toList)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Hakyll
       (Compiler, Configuration(..), Context, Identifier, Item, Routes,
        applyAsTemplate, compile, composeRoutes, compressCssCompiler,
        copyFileCompiler, dateField, defaultContext,
        defaultHakyllReaderOptions, defaultHakyllWriterOptions, field,
        getMetadataField, getResourceBody, gsubRoute, hakyllWith, idRoute,
        itemIdentifier, listField, loadAll, loadAndApplyTemplate,
        lookupString, match, metadataRoute, pandocCompilerWithTransform,
        recentFirst, relativizeUrls, route, setExtension, templateCompiler)
import Text.Pandoc.Definition (Inline(Space, Span, Str), Pandoc)
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
    match "img/*" $ do
        route idRoute
        compile copyFileCompiler

    -- CSS
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
            posts <- recentFirst =<< loadAll "posts/*"
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
    match "posts/*" $ do
        route postsAndDraftsRoutes
        compile $ do
            let subHeadingCtx =
                    field "subHeadingContent" createSubHeadingContentForPost `mappend`
                    postCtx
            pandocOut <-
                pandocCompilerWithTransform
                  defaultHakyllReaderOptions
                  defaultHakyllWriterOptions
                  addSpaceAroundAsciiPandoc
            postTemplateOut <- loadAndApplyTemplate postTemplate subHeadingCtx pandocOut
            applyDefaultTemplate subHeadingCtx postTemplateOut

-- | For posts, add a @date@ field to the default context.
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend`
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

-- | Create the HTML tags for the subheading and "Posted by" lines for
-- a blog post.
--
-- This is kind of hacky.
createSubHeadingContentForPost :: Item a -> Compiler String
createSubHeadingContentForPost item = do
    let ident = itemIdentifier item
    maybeSubHeading <- getMetadataField ident "subHeading"
    maybePostedBy <- getMetadataField ident "postedBy"
    let subHeading = fromMaybe "" maybeSubHeading
        subHeadingHtml = "<h2 class=\"subheading\">" ++ subHeading ++ "</h2>"
        postedBy = fromMaybe "" maybePostedBy
        postedByHtml = "<span class=\"meta\">Posted by " ++ postedBy ++ "</span>"
    return $ subHeadingHtml ++ postedByHtml

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
addSpaceAroundAsciiPandoc :: Pandoc -> Pandoc
addSpaceAroundAsciiPandoc = bottomUp (collapseAsciiSpan . addSpaceAroundAsciiInlines)

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
