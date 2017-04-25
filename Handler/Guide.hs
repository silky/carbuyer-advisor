module Handler.Guide where

import Cache
import Import
import Form ( header
            , withoutMenu
            , footer
            )

getGuideR :: Handler Html
getGuideR = do
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        setTitle "Гид автоскептика"
        $(widgetFile "guide")

