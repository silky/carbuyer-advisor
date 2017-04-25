module Handler.Tax where

import Import hiding (filter)
import Data.List (filter)
import Form
import SessionState
import Database.Persist.Sql

getTaxR :: Handler Html
getTaxR = taxR

postTaxR :: Handler Html
postTaxR = taxR

taxR :: Handler Html
taxR = do
    hyp <- assignHypToIp
    prices <- runDB $ do
      ps <- selectList [AsParamAr ==. (Just hyp), AsParamAa ==. (toSqlKey 2)] []
      prices <- selectList [AsObjectId <-. (fmap (asParamAo . entityVal) ps)] []
      mapM getRelatedService prices
    
    let guideOnly = getPriceOfObj 4 prices
        advices   = getPriceOfObj 5 prices
        combo     = getPriceOfObj 6 prices
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        setTitle "Гид автоскептика"
        $(widgetFile "tax")

getRelatedService p@(Entity pkey price) = do
  (sp:_) <- selectList [AsParamAo ==. pkey, AsParamAa ==. (toSqlKey 1)] [LimitTo 1]
  return (asObjectN price, safeFromJust (toSqlKey 0) . asParamAr $ entityVal sp)

getPriceOfObj obj = safeHead "0.0" . fmap fst . filter (((toSqlKey obj) ==) . snd)