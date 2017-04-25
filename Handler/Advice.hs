{-# LANGUAGE OverloadedStrings #-}
module Handler.Advice where

import Import
import Form
import Database.Persist.Sql (fromSqlKey)
import qualified Data.List as L

getAdviceR :: AdviceId -> Handler Html
getAdviceR adviceId = do
    typs <- runDB $ selectList [] [Asc TypeValue, Asc TypeBottom]
    groups <- runDB $ selectList [] []
    advice <- runDB $ get404 adviceId

    let gt = fmap (typeToFormList . joinTypeWithGroup groups) typs
    let typ = case (adviceType advice) of
                Nothing -> Nothing
                Just tid -> Just $ L.head $ filter ((==tid) . snd) gt
              
    (adviceWidget, enctype) <- generateFormPost 
                               $ (b2AdviceForm (Just $ adviceTitle advice) 
                                   (Just $ adviceContent advice) 
                                   (Just $ adviceType advice) gt (Just $ adviceGroup advice)
                                 (Just $ adviceScore advice))
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        setTitle $ toHtml $ adviceTitle advice
        $(widgetFile "advice")
        $(fayFile "Advice")

postAdviceR :: AdviceId -> Handler Html
postAdviceR adviceId = do
    typs <- runDB $ selectList [] [Asc TypeValue, Asc TypeBottom]
    groups <- runDB $ selectList [] []
    ((res,adviceWidget),enctype) <- runFormPost $ b2AdviceForm Nothing Nothing Nothing (fmap (typeToFormList . joinTypeWithGroup groups) typs) Nothing Nothing
    case res of
         FormSuccess advice -> do
            runDB $ update adviceId 
                      [ AdviceTitle =. (adviceTitle advice)
                      , AdviceContent =. (adviceContent advice)
                      , AdviceType =. (adviceType advice)
                      , AdviceGroup =. (adviceGroup advice)
                      , AdviceScore =. (adviceScore advice)
                      ]
            setMessage $ toHtml $ (adviceTitle advice) <> " created"
            redirect $ AdviceR adviceId
         _ -> defaultLayout $ do
                addStylesheet $ StaticR css_bootstrap_css
                setTitle "Please correct your entry form"
                $(widgetFile "adviceAddError")

deleteAdviceR :: AdviceId -> Handler Html
deleteAdviceR adviceId = do
  runDB $ delete adviceId
  _ <- sendResponseStatus status200 ("DELETED" :: Text)
  redirectUltDest $ AdminR

