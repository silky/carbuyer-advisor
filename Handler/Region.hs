{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Region where

import Import
import Database.Persist.Sql
import Prelude (read)

import qualified Text.JSON as J
import qualified Data.List as L

getRegionR :: Handler Html
getRegionR = do
  regions <- runDB $ selectList [] [Asc RegionId]
  defaultLayout $ do
    addStylesheet $ StaticR css_bootstrap_css
    setTitle "Administration"
    $(widgetFile "region")
    $(fayFile "RegionF")  
  
postRegionR :: Handler Html
postRegionR = do
  result <- runInputPostResult regionForm
  case result of
    FormSuccess (Request t) -> do
        let q = fromResult (J.decode t :: J.Result [URegion])
        rs <- runDB $ selectList [RegionId <-. (fmap (toSqlKey . fromIntegral . rid) q)] []
        mapM_ (updateRegion rs) q
        redirectUltDest AdminR
    _ -> defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        setTitle "Error"
        $(widgetFile "regionUpdateError")

toInt :: String -> Int
toInt a = read a :: Int

updateRegion :: forall site.
                      (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
                      [Entity Region] -> URegion -> HandlerT site IO ()
updateRegion rs (URegion _ rid' rval') = 
  let r = filter ((==(toSqlKey . fromIntegral $ rid')) . entityKey) rs
  in case (L.null r) of
    True  -> liftIO $ return ()
    False -> runDB $ update (entityKey $ L.head r) [RegionPop =. (Just rval')]

fromResult :: J.Result a -> a
fromResult (J.Ok a) = a
fromResult (J.Error a) = error $ "unable decode JSON response: " ++ (a)

data Request = Request String deriving Show

regionForm :: forall (m :: * -> *).
                    (Monad m, RenderMessage (HandlerSite m) FormMessage) =>
                    FormInput m Handler.Region.Request
regionForm = Request
    <$> ireq hiddenField "regionForm"

-- For convenience
(!) :: (J.JSON a) => J.JSObject J.JSValue -> String -> J.Result a
(!) = flip J.valFromObj

data URegion = URegion { inst :: Text, rid :: Int, rvalue :: Text } deriving Show

instance J.JSON URegion where
    showJSON = error "unable to show JSON"
    readJSON (J.JSObject o) = 
        URegion <$> (o ! "instance")
                <*> o ! "rid"
                <*> o ! "rvalue"
    readJSON _ = mzero


