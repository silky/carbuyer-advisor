{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Admin where

import Import
import Form 
import qualified Data.List as L
import Text.Blaze 
import Text.Blaze.Internal hiding (null)

getAdminR :: Handler Html
getAdminR = do
    advices <- runDB $ selectList [] [Desc AdviceTitle]
    a1 <- getAdvicesByLevelName "возраст"
    a2 <- getAdvicesByLevelName "популярность"
    a3 <- getAdvicesByLevelName "цена"
    a4 <- getAdvicesByLevelName "регион"
    groups <- runDB $ selectList [] []
    typs <- runDB $ selectList [] [Asc TypeValue, Asc TypeBottom]

    (adviceWidget, enctype) <- generateFormPost $ b2AdviceForm Nothing Nothing Nothing (fmap (typeToFormList . joinTypeWithGroup groups) typs) Nothing Nothing
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        setTitle "Administration"
        $(widgetFile "advices")


postAdminR :: Handler Html
postAdminR = do
    typs <- runDB $ selectList [] [Asc TypeValue, Asc TypeBottom]
    groups <- runDB $ selectList [] []
    ((res,adviceWidget),enctype) <- runFormPost $ b2AdviceForm Nothing Nothing Nothing (fmap (typeToFormList . joinTypeWithGroup groups) typs) Nothing Nothing
    case res of
         FormSuccess advice -> do
            adviceId <- runDB $ insert advice
            setMessage $ toHtml $ (adviceTitle advice) <> " created"
            redirect $ AdviceR adviceId
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "adviceAddError")

getAdvicesByLevelName :: forall site. (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => Text -> HandlerT site IO [[Entity Advice]]
getAdvicesByLevelName ln = do
  as <- runDB $ do
    ls <- selectList [LevelName ==. ln] []
    ts <- selectList [TypeLevelId <-. (fmap entityKey ls)] []
    selectList [AdviceType <-. (fmap (j . entityKey) ts)] [Asc AdviceGroup, Asc AdviceTitle]
  return $ L.groupBy adviceG as

renderAdvicesByLevelName :: forall (t :: * -> *) a t1. (Foldable t, MonoFoldable (t [Entity Advice]), ToMarkup a) => t [Entity Advice] -> Text -> (Route App -> [t1] -> a) -> MarkupM ()
renderAdvicesByLevelName as (ln::Text) = [hamlet|<ul>
  <span>#{ln}

  $if null as
    <br>
    <span>Пусто пока...
    <br>
  $else
    <br>
    $forall as' <- as
      $maybe ag <- adviceGroup $ entityVal $ L.head $ as'
        <span>#{ag}
      $nothing
        <span>Пустая группа

      <br>
      $forall Entity adviceId advice <- as'
        <li>
          <a href=@{AdviceR adviceId}>#{adviceTitle advice}

|]

j :: forall a. a -> Maybe a
j a = Just a

adviceG :: Entity Advice -> Entity Advice -> Bool
adviceG a b = (adviceGroup $ entityVal a) == (adviceGroup $ entityVal b)
