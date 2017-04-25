module Handler.Terms where

import Import
import Form (footer, header, withoutMenu, allClear)

getTermsR :: Handler Html
getTermsR = do
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        setTitle "Правила автоскептика"
        $(widgetFile "terms")
        $(fayFile    "EditorF")
