
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.TechWithId where

import Cache
import Import
import Form 
import SessionState

getTechWithIdR :: TextAdviseId -> Handler Html
getTechWithIdR textAdviseId = continueTechWithIdR textAdviseId {-do
  uf <- getUrlRender
  let url = uf (TechWithIdR textAdviseId)
  logMessage url
  adminPass TechE url (continueTechWithIdR textAdviseId)-}

continueTechWithIdR :: TextAdviseId -> Handler Html
continueTechWithIdR textAdviseId = do
  c <- getCache
  (images' :: [Entity Image]) <- runDB $ selectList [ImageTextAdviseId ==. textAdviseId] [Asc ImageId]
  let images =  (\x -> zip x [0..]) $ fmap entityVal images'
  let indeces = fmap snd images
  ta <- runDB $ get404 textAdviseId
  let marks = getMarks c
      regions = getRegions c
  (sAdvice':_) <- getRandomSimpleAdvice

  defaultLayout $ do
    addScript $ StaticR js_jquery_min2_js
    addStylesheet $ StaticR css_bootstrap_css
    setTitle "Техническая справка"
    $(widgetFile "techwithid")
    $(fayFile    "HomeF")
    
isEqualToZero = (==0)
