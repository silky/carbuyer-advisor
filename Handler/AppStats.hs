module Handler.AppStats where

import Import

getAppStatsR :: Handler Html
getAppStatsR = do
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        setTitle "Administration"
        $(widgetFile "stats")

