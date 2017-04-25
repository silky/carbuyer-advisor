{-# LANGUAGE RankNTypes #-}
module Auto.Client where

import Import hiding (responseBody, statusCode, responseStatus, statusMessage, responseCookieJar)
import Network.Wreq.Session (Session, getWith)
import Network.Wreq hiding (getWith)
import Control.Lens
import Control.Concurrent (threadDelay)

import qualified Data.Text as T
import qualified Control.Exception as E

data ExtendedResponse = ER
  { arespResult :: Maybe Text
  , arespReferer :: Referer
  , arespCookies :: Maybe CookieJar
  }

type Referer = String

class HttpRequest a where
  getUrl :: a -> String
  getOptions :: a -> Options
  getExtendedOptions :: a -> ExtendedResponse -> Options
  
  performRequest :: Session -> a -> Options -> IO (Maybe Text)
  performRequest sess a ops = do
    let opts = ops
    let url  = getUrl a
    --liftIO $ logMessage $ T.pack $ "[DEBUG] sending request to: " ++ url
    r' <- E.try $ getWith opts sess url
    case r' of
      Left (E.SomeException _) -> return Nothing
      Right r -> do
        --mapM_ logMessage (fmap decodeUtf8 $ showCookies r)
        let statCode = fromIntegral (r ^. responseStatus . statusCode) / 100
            statCode :: Double
        case (statCode / 100) < 4 of
          True  -> return $ fmap (decodeUtf8 . toStrict) $ r ^? responseBody
          False -> return Nothing
  performCustomRequest :: Session -> a -> IO ExtendedResponse
  performCustomRequest sess a = do
    let opts = getOptions a
    let url  = getUrl a
    r' <- E.try $ getWith opts sess url
    case r' of
      Left (E.SomeException _) -> return $ ER Nothing url Nothing
      Right r -> do
        let statCode = fromIntegral (r ^. responseStatus . statusCode) / 100
            statCode :: Double
            cks = r ^. responseCookieJar
        --mapM_ logMessage (fmap decodeUtf8 $ showCookies r)
        case (statCode / 100) < 4 of
          True  -> return $ ER (fmap (decodeUtf8 . toStrict) $ r ^? responseBody) url $ Just cks
          False -> return $ ER Nothing url $ Just cks

class HttpResponse b where
  getResponse :: forall (m :: * -> *). MonadIO m => Maybe Text -> m b
  getCustomResponse :: forall (m :: * -> *). MonadIO m => ExtendedResponse -> m b
    

class (HttpRequest a, HttpResponse b) => HttpClient a b where
  httpAction :: Session -> a -> Options -> IO b
  httpAction sess a ops = do
    b <- performRequest sess a ops
    getResponse b

extendedOptions :: ExtendedResponse -> Options
extendedOptions (ER _ extreferer extcookies) =
  defaults & param "referer" .~ [T.pack extreferer] & cookies .~ extcookies

wait1 :: IO ()
wait1 = threadDelay 300000

showCookies r = r ^? responseHeader "Set-Cookie"
