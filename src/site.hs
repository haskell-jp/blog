{-# LANGUAGE OverloadedStrings #-}

import Data.Default (def)
import Data.Maybe (fromMaybe)
import Hakyll
       (Compiler, Configuration(..), Context, Identifier, Item, Routes,
        applyAsTemplate, compile, composeRoutes, compressCssCompiler,
        copyFileCompiler, dateField, defaultContext, field,
        getMetadataField, getResourceBody, gsubRoute, hakyllWith, idRoute,
        itemIdentifier, listField, loadAll, loadAndApplyTemplate,
        lookupString, match, metadataRoute, pandocCompiler, recentFirst,
        relativizeUrls, route, setExtension, templateCompiler)


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
            pandocOut <- pandocCompiler
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
