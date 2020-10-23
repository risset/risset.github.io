{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Data.Maybe (fromMaybe)
import Hakyll
import Hakyll.Web.Sass

main :: IO ()
main = hakyllWith config $ do
  match "assets/favicon.ico" $ do
    route idRoute
    compile copyFileCompiler

  match "assets/images/*" $ do
    route idRoute
    compile copyFileCompiler

  scssDependency <- makePatternDependency "assets/css/**.scss"
  rulesExtraDependencies [scssDependency]
    $ match "assets/css/main.scss" $ do
        route $ setExtension "css"
        compile (fmap compressCss <$> sassCompiler)

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ do
      pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx = listField "posts" postCtx (return posts) `mappend` defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler
  where
    config = defaultConfiguration
             { previewPort = 5555
             }

dateCtx :: Context String  
dateCtx = dateField "date" "%d-%m-%0Y"

titleCtx :: Context a
titleCtx = field "title" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "No title" $ lookupString "title" metadata

postCtx :: Context String
postCtx = dateCtx `mappend`
          titleCtx `mappend`
          defaultContext
