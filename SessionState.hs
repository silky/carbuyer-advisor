{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SessionState where

import Import
import Control.Lens
import Database.Persist.Sql
import Prelude (read)
import Data.Char (isDigit)

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B

ignoreChars :: Text -> String
ignoreChars = L.filter isDigit . T.unpack
parseInt :: Text -> Int
parseInt = toInt . ignoreChars
toInt :: String -> Int
toInt a = read a :: Int
isInt :: Text -> Bool
isInt = not . null . ignoreChars
digitsAndDots :: Text -> String
digitsAndDots = L.filter (\x -> isDigit x || x == '.') . T.unpack
parseDouble :: Text -> Double
parseDouble = toDouble . digitsAndDots
toDouble :: String -> Double
toDouble a = read a :: Double
isDouble :: Text -> Bool
isDouble = not . null . digitsAndDots

t2b :: Text -> ByteString
t2b = B.pack . T.unpack
b2t :: ByteString -> Text
b2t = T.pack . B.unpack

isNotMaybe :: forall a. Maybe a -> (Bool, Maybe a)
isNotMaybe Nothing = (False, Nothing)
isNotMaybe (Just a) = (True, Just a)
isMaybe :: forall a. Maybe a -> Bool
isMaybe (Just _) = True
isMaybe Nothing  = False

safeHead :: forall a. a -> [a] -> a
safeHead defv [] = defv
safeHead _ (x:_) = x

safeFromJust :: forall t. t -> Maybe t -> t
safeFromJust defv Nothing = defv
safeFromJust _ (Just a) = a

lookupEditor :: forall site. 
  (YesodPersist site, YesodPersistBackend site ~ SqlBackend)
  => Text -> Text -> HandlerT site IO Text
lookupEditor defv key = do
  vals <- runDB $ selectList [AsPropertyK ==. key] [Asc AsPropertyId, LimitTo 1]
  liftIO $ return $ safeHead defv (fmap (asPropertyV . entityVal) vals)

lookupIntEditor :: forall site. 
  (YesodPersist site, YesodPersistBackend site ~ SqlBackend)
  => Text -> HandlerT site IO Text
lookupIntEditor = lookupEditor "0"

getUserDailyLimit :: forall site.
  (YesodPersist site, YesodPersistBackend site ~ SqlBackend)
  => HandlerT site IO Text
getUserDailyLimit = lookupIntEditor "day.tech.limit"
getFreeAmountLimit :: forall site.
  (YesodPersist site, YesodPersistBackend site ~ SqlBackend)
  => Text -> HandlerT site IO Text
getFreeAmountLimit x = lookupIntEditor $ T.concat [ ("free." :: T.Text) , x , (".amount" :: T.Text) ]
getRepostLimit :: forall site. (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => HandlerT site IO Text
getRepostLimit = lookupIntEditor "day.repost.limit"
getIPs :: forall (m :: * -> *). MonadHandler m => m (Maybe ByteString)
getIPs = lookupHeader "X-Real-IP"
getIP :: forall (m :: * -> *). MonadHandler m => m Text
getIP = do
  ips <- getIPs
  liftIO $ return $ b2t $ safeFromJust "0.0.0.0" ips

getUAs :: forall (m :: * -> *). MonadHandler m => m (Maybe ByteString)
getUAs = lookupHeader "User-Agent"
getUA :: forall (m :: * -> *). MonadHandler m => m Text
getUA = do
  uas <- getUAs
  liftIO $ return $ b2t $ safeFromJust "User-Agent: unknown" uas


getSessionState :: forall master. (YesodPersist master, YesodAuth master, AuthId master ~ UserId, YesodPersistBackend master ~ SqlBackend) => Text -> HandlerT master IO SessionState
getSessionState url = do
  mu <- maybeAuthId
  ip <- getIP
  ua <- getUA
  dl <- getUserDailyLimit
  ft <- getFreeAmountLimit "tech"
  fa <- getFreeAmountLimit "advice"
  rl <- getRepostLimit
  let sess =
        SessionState { sMaybeUser = mu
                     , sIP = ip
                     , sUA = ua
                     , dailyLimit = parseInt dl
                     , repostLimit = parseInt rl
                     , watchTechLimit = parseInt ft
                     , watchAdviseLimit = parseInt fa
                     }
  case mu of
    -- Guest
    Nothing -> do
      cw  <- getCurrentWatch
      cr  <- getCurrentRepost
      ciu <- getCurrentIPUA ip ua
      ci  <- getCurrentIP ip
      si  <- isSessionInstalled
      as  <- alreadySeen ip ua url
      let su = SGuest cw cr ciu ci si as
      assignUser sess su
    Just uid -> do
      cd <- getCurrentDaily
      let su = SUser uid cd
      assignUser sess su

assignUser :: forall (m :: * -> *). Monad m => SessionState -> SessionUser -> m SessionState
assignUser sess user = return sess { sUser = user }

getIntCookie :: forall (m :: * -> *). MonadHandler m => Text -> Text -> m Int
getIntCookie t def = do
  cks <- lookupCookie t
  liftIO $ return $ parseInt $ safeFromJust def cks
getCurrentRepost :: forall (m :: * -> *). MonadHandler m => m Int
getCurrentRepost = getIntCookie "r" "0"
getCurrentWatch :: forall (m :: * -> *). MonadHandler m => m Int
getCurrentWatch =  getIntCookie "w" "0"
getCurrentDaily :: forall (m :: * -> *). MonadHandler m => m Int
getCurrentDaily = getIntCookie "d" "0"
getCurrentIPUA :: forall site. (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => Text -> Text -> HandlerT site IO Int
getCurrentIPUA ip ua = do
  let sql = "select count(1) cnt from log where ip = '" <> ip <> "' and ua = '" <> ua <> "'"
  ((res:_) :: [Single Int]) <- runDB $ rawSql sql []
  return $ unSingle res
getCurrentIP :: forall site. (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => Text -> HandlerT site IO Int
getCurrentIP ip = do
  let sql = "select count(1) cnt from log where ip = '" <> ip <> "'"
  ((res:_) :: [Single Int]) <- runDB $ rawSql sql []
  return $ unSingle res
alreadySeen :: forall site. (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => Text -> Text -> Text -> HandlerT site IO Int
alreadySeen ip ua url = do
  let sql = "select count(1) cnt from log where ip = '" <> ip <> "' and ua = '" <> ua <> "' and url='" <> url <> "'"
  ((res:_) :: [Single Int]) <- runDB $ rawSql sql []
  return $ unSingle res

isSessionInstalled :: forall (m :: * -> *). MonadHandler m => m Bool
isSessionInstalled = do
  cks <- lookupCookie "_SESSION"
  return $ fst $ isNotMaybe cks

---------- Hypothesis processing -------------

getRandomHyp = do
  hyps <- runDB $ selectList [AsObjectT ==. (toSqlKey 2)] []
  ((rn:_) :: [Single Int])   <- runDB $ rawSql "with q as (select round(random() :: numeric, 2) ran), q2 as (select row_number() over (order by id) rn, id, q.ran from as_object, q where t = 2), q3 as (select count(1) cnt from q2) select q2.id from q2, q3 where q2.ran between round((q2.rn - 1) :: numeric / q3.cnt, 2) and round(q2.rn :: numeric / q3.cnt, 2)" [] 
  return $ unSingle rn

chooseHyp :: forall a. [a] -> Double -> Int
chooseHyp hyps rand =
  let ln = fromInteger . toInteger $ L.length hyps
      divider = 1.0 / ln
      intervals :: [(Double,Double)]
      intervals = fmap (f divider) [0..(ln - 1)]
      f :: Double -> Double -> (Double, Double)
      f divider = \x -> (x * divider, (x + 1) * divider)
      interval :: Int
      interval = fromInteger $ ceiling $ interval'
      interval' = fst . L.head $ L.filter (g rand) intervals
      g :: Double -> (Double, Double) -> Bool
      g x (y1,y2) = y1 <= x && x < y2
  in interval

assignHypToIp = do
  ip <- getIP
  ((isRegistered:_) :: [Single Int]) <- runDB $ rawSql ("select count(1) from hypothesis where ip = '" <> ip <> "'") []
  case (unSingle isRegistered) of
    0 -> do
      hypId <- getRandomHyp
      (hyp:_) <- runDB $ selectList [AsObjectId ==. (toSqlKey $ fromIntegral hypId)] [LimitTo 1]
      let hypk = entityKey hyp
      runDB $ insert_ $ Hypothesis ip hypk
      return hypk
    _ -> runDB $ do
           hk <- selectList [HypothesisIp ==. ip] [LimitTo 1]
           (o:_) <- selectList [AsObjectId <-. (fmap (hypothesisHyp . entityVal) hk)] [LimitTo 1]
           return $ entityKey o


data LimitedStartPoint = SearchE | TechE
data EndPoint a = GuestPayOrRepost a | UserDailyLimit | GuestNoMoreReposts | Continue a | Captcha a
data Decision = IsUser | UserDailyLimitReached | IsCookieInstalled | IsIPUARegistered | IsIPRegistered | GuestSeenIt | GuestFreeLimitReached | GuestReposted | GuestRepostLimitReached 
data SessionState =
  SessionState { sMaybeUser :: Maybe UserId
               , sUser :: SessionUser
               , sIP :: Text
               , sUA :: Text
               , sLimitReached :: Bool
               , dailyLimit :: Int
               , repostLimit :: Int
               , watchTechLimit :: Int
               , watchAdviseLimit :: Int
               }
data SessionUser =
  SUser { sUid :: UserId
        , sCurrentDaily :: Int
        }
  | SGuest { sCurrentWatch :: Int
           , sCurrentRepost :: Int
           , sCurrentIPUA :: Int
           , sCurrentIP :: Int
           , sSessionInstalled :: Bool
           , sAlreadySeen :: Int
           }
makeLenses ''SessionState
makeLenses ''SessionUser
