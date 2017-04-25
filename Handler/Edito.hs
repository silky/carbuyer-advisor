module Handler.Edito where

import Import

getEditoR :: Handler Html
getEditoR = do
  ps <- runDB $ selectList [] [Asc AsPropertyId]
  defaultLayout $ do
    addStylesheet $ StaticR css_bootstrap_css
    setTitle "Редактор атрибутов"
    $(widgetFile "editor")
    $(fayFile "EditorF")
