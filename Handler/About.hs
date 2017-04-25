module Handler.About where

import Cache
import Import
import Form ( header
            , withoutMenu
            , allClear
            , footer
            )

getAboutR :: Handler Html
getAboutR = do
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        setTitle "Об автоскептике"
        $(widgetFile "about")
