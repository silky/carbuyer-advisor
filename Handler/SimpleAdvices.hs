module Handler.SimpleAdvices where

import Import
import Form 

getSimpleAdvicesR :: Handler Html 
getSimpleAdvicesR = do
    advices <- runDB $ selectList [] [Desc SimpleAdviceId]

    (adviceWidget, enctype) <- generateFormPost $ b2SimpleAdviceForm Nothing
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        setTitle "Administration: Simple good advices"
        $(widgetFile "simple-advices")

postSimpleAdvicesR :: Handler Html 
postSimpleAdvicesR = do
    ((res,adviceWidget),enctype) <- runFormPost $ b2SimpleAdviceForm Nothing
    case res of
         FormSuccess advice -> do
            adviceId <- runDB $ insert advice
            redirect SimpleAdvicesR
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "adviceAddError")
