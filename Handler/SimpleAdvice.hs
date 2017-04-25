module Handler.SimpleAdvice where

import Import
import Form
import Database.Persist.Sql (fromSqlKey)

getSimpleAdviceR :: SimpleAdviceId -> Handler Html
getSimpleAdviceR simpleAdviceId = do
    advice <- runDB $ get404 simpleAdviceId
    (adviceWidget, enctype) <- generateFormPost 
                               $ (b2SimpleAdviceForm (Just $ simpleAdviceContent advice))
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        $(widgetFile "simple-advice")
        $(fayFile "SimpleAdvice")


postSimpleAdviceR :: SimpleAdviceId -> Handler Html
postSimpleAdviceR simpleAdviceId = do
    ((res,adviceWidget),enctype) <- runFormPost $ b2SimpleAdviceForm Nothing
    case res of
         FormSuccess advice -> do
            runDB $ update simpleAdviceId 
                      [ SimpleAdviceContent =. (simpleAdviceContent advice)
                      ]
            redirect SimpleAdvicesR
         _ -> defaultLayout $ do
                addStylesheet $ StaticR css_bootstrap_css
                setTitle "Please correct your entry form"
                $(widgetFile "adviceAddError")

deleteSimpleAdviceR :: SimpleAdviceId -> Handler Html
deleteSimpleAdviceR simpleAdviceId = do
  runDB $ delete simpleAdviceId
  _ <- sendResponseStatus status200 ("DELETED" :: Text)
  redirectUltDest  SimpleAdvicesR
