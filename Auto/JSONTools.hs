{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Auto.JSONTools where

import Import hiding (pack, concat)
import Data.Text (pack, breakOn, concat)
import HSH.Command

import qualified Text.JSON as J
import qualified Control.Exception as E

type ErrorText = Text

(!) :: (J.JSON a) => J.JSObject J.JSValue -> String -> J.Result a
(!) = flip J.valFromObj

fromResult :: J.Result a -> Either Text a
fromResult (J.Ok a) = Right a
fromResult (J.Error a) = Left (pack $ "unable decode JSON response: " ++ a)

phantomGetAllAdCount :: App -> String -> IO (Either ErrorText Text)
phantomGetAllAdCount app url = do
  let u = "phantomjs --cookies-file=cookie-auto-ru.txt pjs/count.js '" <> url <> "?sort_offers=cr_date-DESC&beaten=0&image=false&page_num_offers=1'"
  logMessageSite app $ pack u
  res <- try $ liftIO $ run (u :: String)
  case res of
    Left (E.SomeException _) -> return $ Left "Проиозшла ошибка при получении удаленной информации"
    Right (val :: String) -> return $ Right (pack val)

phantomGetAdsByRegion :: App -> Entity Region -> String -> IO (Either ErrorText Text)
phantomGetAdsByRegion app (Entity _ r) url = do
  let u = "phantomjs --cookies-file=pcookies/cookie-" <> (regionIso r) <> ".txt pjs/load.js '" <> (pack url) <> "?beaten=0&image=false&customs_state=ALL&sort_offers=cr_date-DESC&page_num_offers=1'"
  logMessageSite app $ "Region: " <> (regionName r)
  logMessageSite app $ u
  res <- try $ liftIO $ run (unpack u :: String)
  case res of
    Left (E.SomeException _) -> return $ Left "Проиозшла ошибка при получении удаленной информации"
    Right (val :: String) -> return $ Right (pack val)

phantomGetAdsByYear :: App -> Entity Region -> String -> String -> IO (Either ErrorText Text)
phantomGetAdsByYear app (Entity _ r) url year = do
  let (befu,aftu) = breakOn "/used" $ pack url
      url' = concat [befu , "/" , pack year , "-year" ,  aftu ]
      u = "phantomjs --cookies-file=pcookies/cookie-" <> (regionIso r) <> ".txt pjs/load.js '" <> url' <> "?beaten=0&image=false&customs_state=ALL&sort_offers=cr_date-DESC&page_num_offers=1'"
  logMessageSite app $ u
  res <- try $ liftIO $ run (unpack u :: String)
  case res of
    Left (E.SomeException _) -> return $ Left "Проиозшла ошибка при получении удаленной информации"
    Right (val :: String) -> return $ Right (pack val)
