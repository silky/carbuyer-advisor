{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Monitoring where

import Import hiding (pack)
import HSH.Command
import Data.Text (pack)

getMonitoringR :: Handler Html
getMonitoringR = do
    (ram :: String) <- liftIO $ run ("sar -r" :: String)
    (hdd :: String) <- liftIO $ run ("df -h" :: String)
    
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        setTitle "Administration"
        $(widgetFile "monitoring")
