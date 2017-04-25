{-# LANGUAGE OverloadedStrings #-}
module Handler.TextAdvise where

import Cache
import Import hiding (null)
import Form
import Database.Persist.Sql (fromSqlKey)
import Data.List ((!!), null, nub)

import qualified Data.Text as T

getTextAdviseR :: TextAdviseId -> Handler Html
getTextAdviseR textAdviseId = do
  ta <- runDB $ get404 textAdviseId
  addedGs <- runDB $ do
    lks <- selectList [LkTagTextAdviseId ==. textAdviseId] []
    gs <- selectList [GenerationId <-. (fmap (lkTagGeneration . entityVal) lks)] [Asc GenerationGeneration]
    return $ gs
  addedImgs <- runDB $ selectList [ImageTextAdviseId ==. textAdviseId] [Asc ImageId]
  gens <- generationsToHtml ta
  (textAdviseWidget, enctype) <- generateFormPost
                                 $ (b2TextAdviseForm (Just $ textAdviseName ta)
                                     (Just $ textAdviseHappyText ta)
                                     (Just $ textAdviseUnhappyText ta)
                                     (Just $ textAdviseLookupText ta)
                                     (Just $ textAdvisePromo ta))
  defaultLayout $ do
    addStylesheet $ StaticR css_bootstrap_css
    setTitle $ toHtml $ textAdviseName ta
    $(widgetFile "text-advise")
    $(fayFile "TextAdvise")

postTextAdviseR :: TextAdviseId -> Handler Html
postTextAdviseR textAdviseId = do
    ta <- runDB $ get404 textAdviseId

    ((res,textAdviseWidget),enctype) <- runFormPost $ b2TextAdviseForm Nothing Nothing Nothing Nothing Nothing  
    case res of
         FormSuccess advise -> do
            runDB $ update textAdviseId 
                      [ TextAdviseName =. (textAdviseName advise)
                      , TextAdviseHappyText =. (textAdviseHappyText advise)
                      , TextAdviseUnhappyText =. (textAdviseUnhappyText advise)
                      , TextAdviseLookupText =. (textAdviseLookupText advise)
                      , TextAdvisePromo =. (textAdvisePromo advise)
                      ]
            setMessage $ toHtml $ (textAdviseName advise) <> " updated"
            redirect $ TextAdviseR textAdviseId
         _ -> defaultLayout $ do
                addStylesheet $ StaticR css_bootstrap_css
                setTitle "Please correct your entry form"
                $(widgetFile "textAdviseAddError")

deleteTextAdviseR :: TextAdviseId -> Handler Html
deleteTextAdviseR textAdviseId = do
  runDB $ do
    ls <- selectList [LkTagTextAdviseId ==. textAdviseId] []
    mapM_ delete $ fmap entityKey ls
    delete textAdviseId
  _ <- sendResponseStatus status200 ("DELETED" :: Text)
  redirectUltDest $ ImportR

generationsToHtml ::
  TextAdvise -> HandlerT App IO [(Text, Key Generation)]
generationsToHtml advise = do
  cache <- getCache
  return $ fmap rgToTuple . filterByAdvise advise $ generationList cache

data RenderedGeneration =
  RenderedGeneration
  { fullName :: Text
  , gId :: Key Generation
  }

rgToTuple :: RenderedGeneration -> (Text, Key Generation)
rgToTuple (RenderedGeneration fn g) = (fn, g)

generationList :: DomainData -> [RenderedGeneration]
generationList (DomainData mas mos _ _ gens _ _ _ _) =
  fmap (transformGeneration mas mos) gens

filterByAdvise :: TextAdvise -> [RenderedGeneration] -> [RenderedGeneration]
filterByAdvise (TextAdvise _ fn _ _ _ _) gs =
  if (null $ res)
  then gs
  else res
  where getFirstWord = T.toLower . T.takeWhile (/= ' ')
        f y x = T.isPrefixOf (getFirstWord . fullName $ x) y
        res = filter (f fn) gs

transformGeneration :: [Entity Mark]
  -> [Entity Model]
  -> Entity Generation
  -> RenderedGeneration
transformGeneration mas mos (Entity gid (Generation _ _ _ _ ta ba n maid moid)) =
  RenderedGeneration
    { fullName = fn, gId = gid }
  where filterBy x = filter ((==x) . entityKey)
        extractBy f = f . entityVal . (!! 0)
        mark' = extractBy markName . filterBy maid $ mas
        model' = extractBy modelName . filterBy moid $ mos
        fn = T.concat [mark', " -- ", model', " -- ", fromMaybeText n, " (", fromMaybeInt ba, "-", fromMaybeInt ta, ")"]
        fromMaybeText Nothing = "I"
        fromMaybeText (Just a) = a

fromMaybeInt :: forall a. Show a => Maybe a -> Text
fromMaybeInt Nothing = "-"
fromMaybeInt (Just a) = T.pack $ show a

fromMaybeText :: Maybe Text -> Text
fromMaybeText Nothing = ""
fromMaybeText (Just a) = a
