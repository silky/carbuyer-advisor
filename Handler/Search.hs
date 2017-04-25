{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Search where

import Import hiding (Response, null, head, sum)
import Cache
import Database.Persist.Sql
import Data.List (nub, null, head, sum)
import Auto.Client
import Auto.Parser
import Auto.Algorythm
import Auto.JSONTools
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Control.Lens
import Data.Char (isDigit)
import Form
import SessionState
import Yesod.Auth.Email (registerR)

import qualified Network.Wreq as W
import qualified Text.JSON as J
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Control.Exception as E

data InitRequest = IR RegionIdS MarkIdS ModelIdS YearIdS GenerationIdS BudgetS deriving (Eq, Show)

data Request' = R RegionId' MarkId' ModelId' YearId GenerationId' Budget deriving Show
data AutoRequest =
  R2 { arRegion :: Entity Region
     , arMark :: MarkO
     , arModel :: ModelO
     , arYear :: YearO
     , arIsGenerationNeeded :: IsGenerationNeeded
     , arBudget :: Budget
     , arGeneration :: Entity Generation
     }
type ErrorMsg = Text

instance HttpRequest AutoRequest where
  getUrl ar =
    let t = generationAutoUrl . entityVal $ arGeneration ar
    in case T.take 4 t of
         "http"  -> T.unpack t
         _ -> T.unpack $ T.concat [ "https://m.auto.ru" , t ]
  getOptions _ = W.defaults

instance HttpClient AutoRequest B.ByteString 

instance HttpResponse B.ByteString where
  getResponse Nothing = liftIO $ do
    return ""
  getResponse (Just x) = liftIO $ do
    writeFile "output.html" x
    return $ encodeUtf8 x


checkRequest :: InitRequest -> Either ErrorMsg Request'
checkRequest ir@(IR ridS maIdS moIdS yIdS gIdS bS) = 
  let generationToInt :: Text -> Int
      generationToInt gid =
        if isInt gid 
        then parseInt gid
        else 0
  in if (and $ fmap (isInt) [ridS, maIdS, moIdS, yIdS, bS]) && ir /= emptyInitRequest 
     then Right $ R (parseInt ridS)
                    (parseInt maIdS)
                    (parseInt moIdS)
                    (parseInt yIdS)
                    (generationToInt gIdS)
                    (parseInt bS)
     else if ir /= emptyInitRequest 
          then Left "Похоже, вы ввели неправильные данные. Попробуйте ещё раз. Некоторые поля заполнены некорректно."
          else Left "Похоже, вы ввели неправильные данные. Попробуйте ещё раз. Некоторые поля не были заполнены."

proxyRequest :: Request' -> HandlerT App IO AutoRequest
proxyRequest (R rid maId moId yId gId b) = do
      c <- getCache
      let r = head $ filter (byKey rid . entityKey) $ getRegions c
          ma = getCheckAutoUrlPart markAutoUrl maId getMarks c ""
          mo = getCheckAutoUrlPart modelAutoUrl moId getModels c ""
          y = getCheckAutoUrlPart (T.pack . show . ageAge) yId getAges c ""
          gs = getGenerations c
          gs' = filter (byKey gId . entityKey) gs
          currentAge = head
                       $ fmap (ageAge . entityVal)
                       $ filter (byKey yId . entityKey)
                       $ getAges c
          currentGens = filter (byKey moId . generationModelId . entityVal) gs
          g = if ign
              then head
                   $ filter ((<= (Just currentAge)) . generationBottomAge . entityVal)
                   $ filter ((>= (Just currentAge)) . generationTopAge . entityVal) currentGens
              else head gs'
          ign = gId == 0 || null gs'
          
      return $ (R2 r ma mo y ign b g)

getAutoUrlPart ::
  forall (f :: * -> *) b b1 record r t.
    ( Integral r, Functor f, ToBackendKey SqlBackend record
    , IsSequence (f (Entity b1)), Element (f (Entity b1)) ~ Entity record
    ) => (b1 -> b) -> r -> (t -> f (Entity b1)) -> t -> f b
getAutoUrlPart fEntityAttr comparingVal cachePart cache =
  fmap (fEntityAttr . entityVal)
  $ filter (byKey comparingVal . entityKey)
  $ cachePart cache

checkAutoUrlPart :: forall t. [t] -> t -> t
checkAutoUrlPart [] a = a
checkAutoUrlPart (a:_) _ = a

getCheckAutoUrlPart ::
  forall c record r t.
  (Integral r, ToBackendKey SqlBackend record)
  => (record -> c) -> r -> (t -> [Entity record]) -> t -> c -> c
getCheckAutoUrlPart fEntityAttr comparingVal cachePart cache alternative =
  flip checkAutoUrlPart alternative
  $ getAutoUrlPart fEntityAttr comparingVal cachePart cache

type RegionId' = Int
type MarkId' = Int
type ModelId' = Int
type YearId = Int
type GenerationId' = Int
type Budget = Int

type RegionUrl' = Text
type MarkO = Text
type ModelO = Text
type YearO = Text
type GenerationO = Text

type RegionIdS = Text
type MarkIdS = Text
type ModelIdS = Text
type YearIdS = Text
type GenerationIdS = Text
type BudgetS = Text

type Offset = Int
type IsGenerationNeeded = Bool

data Response' = Response' String deriving Show

data PhLoadResponse =
  PhLoadResponse
  { phTotal :: Int
  , phPrices :: [Text]
  } deriving Show

data PhCountResponse =
  PhCountResponse
  { phCountTotal :: Int
  } deriving Show

itemForm :: forall (m :: * -> *).
                  (Monad m, RenderMessage (HandlerSite m) FormMessage) =>
                  FormInput m Response'
itemForm = Response'
    <$> ireq hiddenField ("q" :: Text)

instance J.JSON Response' where
  showJSON = error "unable to show JSON"
  readJSON (J.JSObject o) = Response' <$> o ! "instance"
  readJSON _ = mzero

instance J.JSON Request' where
  showJSON = error "unable to show JSON"
  readJSON (J.JSObject o) =
    R <$> o ! "slot1"
      <*> o ! "slot2"
      <*> o ! "slot3"
      <*> o ! "slot4"
      <*> o ! "slot5"
      <*> o ! "slot6"
  readJSON _ = mzero      

instance J.JSON InitRequest where
  showJSON = error "unable to show JSON"
  readJSON (J.JSObject o) =
    IR <$> o ! "slot1"
       <*> o ! "slot2"
       <*> o ! "slot3"
       <*> o ! "slot4"
       <*> o ! "slot5"
       <*> o ! "slot6"
  readJSON _ = mzero      

instance J.JSON PhLoadResponse where
  showJSON = error "unable to show JSON"
  readJSON (J.JSObject o) =
    PhLoadResponse <$> o ! "total"
       <*> o ! "prices"
  readJSON _ = mzero      

instance J.JSON PhCountResponse where
  showJSON = error "unable to show JSON"
  readJSON (J.JSObject o) =
    PhCountResponse <$> o ! "total"
  readJSON _ = mzero      

safeFromResult :: J.Result InitRequest -> InitRequest
safeFromResult (J.Ok a) = a
safeFromResult (J.Error _) = emptyInitRequest

--------------------------

instance TagRep B.ByteString where 
  toTagRep x = case parseTags x of
                 [a] -> toTagRep a
                 _ -> error $ B.unpack $ B.append "When using a TagRep it must be exactly one tag, you gave: " x

instance Parseable B.ByteString Counter where
  toExtractedObject ctags = Counter cnt
    where tags = extractTags (patternBegin autoCounterOpts) (patternEnd autoCounterOpts) ctags
          pre' = B.filter isDigit $ innerText tags
          cnt = case pre' of
            "" -> 0
            _  -> parseInt $ T.pack (B.unpack pre')

  opts = autoCounterOpts

instance Parseable B.ByteString AutoPrice where
  toExtractedObject ctags = AutoPrice price
    where tags = extractTags patternBegin' patternEnd' ctags
          pre' = B.filter isDigit $ innerText tags
          price = case pre' of
            "" -> 0
            _  -> parseInt $ T.pack (B.unpack pre')
          patternBegin' = patternBegin autoPriceOpts
          patternEnd'   = patternEnd autoPriceOpts

  toExtractedList ops ts = b
    where tags = extractTags patternBegin' patternEnd' ts
          e = partitions (~== part) tags
          b = fmap toExtractedObject e

          part = partitionEdge ops
          patternBegin' = scopeBegin ops
          patternEnd'   = scopeEnd ops

  opts = autoPriceOpts

autoPriceOpts :: ParsingOptions B.ByteString
autoPriceOpts = ParsingOptions
  { scopeBegin = "<div class=\"listing__results\">"
  , scopeEnd = "<footer data-bem=\"{&quot;footer&quot;:{}}\" class=\"footer i-bem footer_js_inited\">"
  , partitionEdge = "<div class=\"listing__item\">"
  , patternBegin = "<a class=\"listing__item-price\">"
  , patternEnd = "</a>"
  }

autoCounterOpts :: ParsingOptions B.ByteString
autoCounterOpts = ParsingOptions
  { scopeBegin = ""
  , scopeEnd = ""
  , partitionEdge = ""
  , patternBegin = "<div class=\"listing__found\">"
  , patternEnd   = "</div>"
  }

newtype Counter = Counter { unCounter :: Int }

newtype AutoPrice = AutoPrice { unPrice :: Int }

--------------------------

adviseCounter :: forall site a.
                       (YesodPersist site, RawSql a,
                        YesodPersistBackend site ~ SqlBackend) =>
                       Counter -> HandlerT site IO [a]
adviseCounter (Counter cntr) = do
  let sql =
        T.concat [ "select ?? from advice where type in (select id from type where level_id = 2 and "
                 , T.pack $ show cntr, " between bottom and top)"
                 ]
  res <- runDB $ rawSql sql []
  return res

advisePrice :: forall site a.
                     (YesodPersist site, RawSql a,
                      YesodPersistBackend site ~ SqlBackend) =>
                     Score -> HandlerT site IO [a]
advisePrice score = do
  let sql =
        T.concat [ "with types as ("
                 , "      select count(*) cnt, type"
                 , "      from advice"
                 , "      where type in ("
                 , "          select id from type where level_id = 3"
                 , "      )"
                 , "      group by type"
                 , " )"
                 , " select ?? from ("
                 , " ("
                 , "    select a.* from advice a, types t"
                 , "    where a.type = t.type and t.cnt > 1 and t.type = "
                 , choosePriceAdvice score
                 , " "
                 , "    offset floor(random()) limit 1"
                 , "  )"
                 , " union all"
                 , " select a.* from advice a, types t where a.type = t.type and t.cnt = 1 and t.type = "
                 , choosePriceAdvice score
                 , ") advice"
                 ]
  res <- runDB $ rawSql sql []
  return res

choosePriceAdvice :: Score -> Text
choosePriceAdvice OutLeft  = "9"
choosePriceAdvice Minus20  = "15"
choosePriceAdvice Minus10  = "14"
choosePriceAdvice Equal    = "7"
choosePriceAdvice Plus10   = "13"
choosePriceAdvice Plus20   = "8"
choosePriceAdvice Plus30   = "10"
choosePriceAdvice OutRight = "12"

adviseAge :: forall site a.
                   (YesodPersist site, RawSql a,
                    YesodPersistBackend site ~ SqlBackend) =>
                   Entity Age -> HandlerT site IO [a]
adviseAge age = do
  let sql =
        T.concat [ "select ?? from advice where type in (select id from type where level_id = 1 and date_part('year', now() - interval '"
                 , T.pack $ show age'
                 , " years') between bottom and top)"
                 ]
      age' = ageAge $ entityVal age
  res <- runDB $ rawSql sql []
  return res

--------------------------
getSearchR :: Handler Html
--getSearchR = redirect HomeR
getSearchR = redirect HomeR

postSearchR :: Handler Html
postSearchR = continuePostResult {-do
  uf <- getUrlRender
  let url = uf SearchR
  adminPass SearchE url (continuePostResult)-}

continuePostResult :: Handler Html
continuePostResult = do
  app <- getYesod
  result <- runInputPostResult itemForm
  (sAdvice':_) <- getRandomSimpleAdvice

  case result of
    FormSuccess (Response' t) -> do
      query' <- decodeRequest t
      liftIO $ logMessageSite app ("[DEBUG] 0. decoding request" :: Text)
      let pquery = checkRequest query'
      liftIO $ logMessageSite app ("[DEBUG] 1. checking request" :: Text)

      case pquery of
        Left emsg -> renderError ("[DEBUG] 2. failed to check" :: Text) emsg
        Right query -> do
          logMessage ("[DEBUG] 3. Succeeded to check" :: Text)
          c <- getCache
          let regions = getRegions c
              marks   = getMarks c
              rs = entityFromCache regionQ query getRegions c
              as = entityFromCache ageQ query getAges c
              mas = entityFromCache markQ query getMarks c
              mos = entityFromCache modelQ query getModels c
              -- gs = getGenerations c
              rsg = nub $ fmap (regionPop . entityVal) $ rs

          case or $ [null rs,null as,null mas,null mos] of
            True  -> do
              let emsg = "Похоже, вы ввели неправильные данные. Попробуйте ещё раз. Некоторые поля не были заполнены." :: String
              renderError ("[DEBUG] 4. failed to return data" :: Text) emsg
            False -> do
              let a  = head as
                  ma = head mas
                  mo = head mos
                  r  = head rs
              typs <- runDB $ selectList [TypeLevelId ==. (toSqlKey 4)] []
              (adv:_) <- runDB
                         $ selectList [ AdviceGroup <-. rsg
                                      , AdviceType <-. (fmap ( toMaybe . entityKey) typs)] []
  
              autoRequest <- proxyRequest query

              t <- liftIO $ getCurrentTime
              irid <- runDB $ insert $ logInitRequest t query

              let url = getUrl autoRequest
                  y   = show $ ageAge $ entityVal a

              autoCountResult <- liftIO $ phantomGetAllAdCount app url
              let autoCountResponse'' = case autoCountResult of
                    Left _  -> "{total:0}"
                    Right b -> b
                  autoCountResponse' = fromResult
                    (J.decode (T.unpack autoCountResponse'') :: J.Result PhCountResponse)
                  autoCountResponse = case autoCountResponse' of
                    Left _ -> emptyAutoCountResponse
                    Right b -> b
                  cntr = Counter (phCountTotal autoCountResponse)
              autoLoadResult <- liftIO $ case cntr of
                Counter 0 -> return $ Left "Counter is already equal to null"
                _         -> phantomGetAdsByYear app r url y
              logMessage $ "Total count: " <> (T.pack $ show $ phCountTotal autoCountResponse)
              logMessage ("interaction with phantom ended" :: Text)
              let autoLoadResponse' = fromResult $ (J.decode (T.unpack autoLoadResponse'') :: J.Result PhLoadResponse)
                  autoLoadResponse'' = case autoLoadResult of
                    Left  _ -> "{total:0, prices:[]}"
                    Right b -> b
                  autoLoadResponse = case autoLoadResponse' of
                    Left  _ -> emptyAutoLoadResponse
                    Right b -> b
                  g = arGeneration autoRequest
                  autoprices = fmap (AutoPrice <$> parseInt) $ phPrices autoLoadResponse
                  autoprices :: [AutoPrice]
                  mprice :: Double
                  mprice = mean $ fmap (unPrice) autoprices
                  amprice = round mprice :: Int
                  score :: Score
                  score = determineScore (arBudget autoRequest) mprice
                  mres = mprice < (1 :: Double) || isNaN mprice
              t2 <- liftIO $ getCurrentTime 
              runDB $ insert_ $ LogResponse t2 irid (phCountTotal autoCountResponse) amprice (T.pack $ show score)
              logMessage $ T.pack $ "[DEBUG] total: " ++ show (unCounter cntr)
              logMessage $ T.pack $ "[DEBUG] mean price is:  " ++ show mprice
              logMessage $ T.pack $ "[DEBUG] score is: " ++ show score
              advC :: [Entity Advice] <- adviseCounter cntr
              advP :: [Entity Advice] <- advisePrice score
              advA :: [Entity Advice] <- adviseAge a
              let risc = sum $ concat $ fmap (fmap $ adviceScore . entityVal)
                         [advC, advP, advA]

              defaultLayout $ do
                addStylesheet $ StaticR css_bootstrap_css
                setTitle $ toHtml $
                  showQueryGen ma mo g
                $(widgetFile "search")
                $(fayFile "HomeF")
    _ -> redirect HomeR

renderError debugMsg msg = do
  c <- getCache
  let regions = getRegions c
      marks   = getMarks c
      emsg = msg
  logMessage debugMsg 
  defaultLayout $ do
    setTitle "Неправильно заполнена форма"
    $(widgetFile "error")
    $(fayFile "ErrorF")
  
regionQ :: forall t. Num t => Request' -> t
ageQ :: forall t. Num t => Request' -> t
markQ :: forall t. Num t => Request' -> t
modelQ :: forall t. Num t => Request' -> t
generationQ :: forall t. Num t => Request' -> t

regionQ (R x _ _ _ _ _) = fromIntegral x
ageQ (R _ _ _ x _ _) = fromIntegral x
generationQ (R _ _ _ _ x _) = fromIntegral x
markQ (R _ x _ _ _ _) = fromIntegral x
modelQ (R _ _ x _ _ _) = fromIntegral x

entityFromCache :: forall t record t1 r. (ToBackendKey SqlBackend record, IsSequence r,
                          Element r ~ Entity record) =>
                         (t -> Int64) -> t -> (t1 -> r) -> t1 -> r
entityFromCache fromResponse resp fromCache cache =
  filter ((==(fromResponse resp)) . fromSqlKey . entityKey) $ fromCache cache

toMaybe :: forall a. a -> Maybe a
toMaybe a = Just a

decodeRequest ::
  forall (m :: * -> *).
  (MonadHandler m, MonadBaseControl IO m, HandlerSite m ~ App) =>
  String -> m InitRequest
decodeRequest req = do
  res <- try $ return $ safeFromResult (J.decode req :: J.Result InitRequest)
  case res of
    Left (E.SomeException _) -> do
      logMessage ("exception caught" :: Text)
      return emptyInitRequest
    Right val -> return val

emptyInitRequest :: InitRequest
emptyInitRequest = IR "" "" "" "" "" ""

emptyAutoLoadResponse :: PhLoadResponse
emptyAutoLoadResponse = PhLoadResponse 0 []

emptyAutoCountResponse :: PhCountResponse
emptyAutoCountResponse = PhCountResponse 0

data AutoResponse = AR1 PhCountResponse | AR2 PhLoadResponse

logInitRequest :: UTCTime -> Request' -> LogRequest
logInitRequest t (R r ma mo y g b) = LogRequest t (f r) (f ma) (f mo) (f y) (f' g) b
  where f = toSqlKey . fromIntegral
        f' x = if (x==0) then Nothing else Just (f x)

isEastRegion :: Entity Region -> Bool
isEastRegion = (==1) . regionEast . entityVal
