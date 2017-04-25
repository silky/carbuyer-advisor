{-# LANGUAGE OverloadedStrings #-}
module Handler.Type where

import Import
import Data.Maybe (fromJust)
import Form hiding (fromJust)
import Database.Persist.Sql

getTypeR :: TypeId -> Handler Html
getTypeR typeId = do
    typ <- runDB $ get404 typeId
    lev <- runDB $ get404 $ typeLevelId typ
    grp <- runDB $ get404 $ typeGroupId typ
    (typeWidget, enctype) <- generateFormPost 
                              ( b2TypeForm 
                                  (Just $ typeLevelId typ)
                                  (Just $ typeGroupId typ)
                                  (Just $ typeTop typ)
                                  (Just $ typeBottom typ)
                                  (Just $ typeValue typ)
                              )
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        setTitle "Пограничные условия"
        $(widgetFile "type") 
        $(fayFile "Types")

postTypeR :: TypeId -> Handler Html
postTypeR typeId = do
    ((res,typeWidget),enctype) <- runFormPost $ bootstrapTypeForm
    case res of
         FormSuccess type' -> do
            runDB $ update typeId 
                      [ TypeLevelId =. (typeLevelId type')
                      , TypeGroupId =. (typeGroupId type')
                      , TypeTop     =. (typeTop   type')
                      , TypeBottom  =. (typeBottom type')
                      , TypeValue   =. (typeValue   type')
                      ]
            setMessage $ "Type created"
            redirect $ TypeR typeId
         _ -> defaultLayout $ do
                addStylesheet $ StaticR css_bootstrap_css
                setTitle "Please correct your entry form"
                $(widgetFile "typeAddError")

deleteTypeR :: TypeId -> Handler Html
deleteTypeR typeId = do
  a <- runDB $ selectList [AdviceType ==. (Just typeId)] []
  case (null a) of
    True -> do
      runDB $ delete typeId
      _ <- sendResponseStatus status200 ("DELETED" :: Text)
      redirectUltDest $ TypesR
    False -> do
      _ <- sendResponseStatus status204 ("NO CONTENT" :: Text)
      redirectUltDest $ TypesR
                       

