{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module Auto.Adv where

import Import hiding (responseBody, statusCode, responseStatus, statusMessage)
import Auto.Parser
import Auto.Client
import Text.HTML.TagSoup
import Prelude (read)
import Data.Char (isDigit)
import Control.Lens

import qualified Data.ByteString.Char8 as B
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as Sess
import qualified Data.Text as T

data AdCounter = AC { ac :: Int } deriving Show
data AdPrice = AP { apr :: Int } deriving Show

data AutoRequest = R2 RegionUrl' MarkO ModelO YearO GenerationO Offset IsGenerationNeeded

type CounterBS = B.ByteString
type PriceBS = B.ByteString
type URL = B.ByteString
type RegionUrl' = Text
type MarkO = Text
type ModelO = Text
type YearO = Text
type GenerationO = Text
type Offset = Int
type IsGenerationNeeded = Bool

instance HttpRequest AutoRequest where
  getUrl (R2 r _ mo _ g _ ign) =
    if ign
    then T.unpack $ T.concat [ r, g ]
    else case T.take 4 mo of
      "http"  -> T.unpack mo
      "" -> "http://auto.ru/"
      _ -> T.unpack $ T.concat [ r, mo ]
  getOptions (R2 _ _ _ _ _ 0 _) = W.defaults & W.param "sort_offers" .~ ["price-ASC"]
  getOptions (R2 _ _ _ _ _ i _) = W.defaults & W.param "page_num_offers" .~ [T.pack . show $ (min 1 i)]
  getExtendedOptions a (ER _ referer cks) = (getOptions a) & W.cookies .~ cks & W.param "referer" .~ [T.pack referer]

setCookies :: Maybe CookieJar -> W.Options
setCookies c = W.defaults & W.cookies .~ c

instance HttpClient AutoRequest B.ByteString

instance HttpRequest B.ByteString where
  getUrl a = B.unpack a
  getOptions _ = W.defaults

instance HttpResponse B.ByteString where
  getResponse Nothing = liftIO $ do
    return ""
  getResponse (Just x) = liftIO $ do
    writeFile "output.html" x
    return $ encodeUtf8 x
  getCustomResponse (ER Nothing _ _) = return ""
  getCustomResponse (ER (Just a) _ _) = return $ encodeUtf8 a

instance HttpClient CounterBS AdCounter

instance HttpResponse AdCounter where
  getResponse Nothing = liftIO $ do
    return $ AC 0
  getResponse (Just x) = liftIO $ do
    let ctags = getTags $ encodeUtf8 x
    return $ toExtractedObject ctags

instance TagRep B.ByteString where 
  toTagRep x = case parseTags x of
                 [a] -> toTagRep a
                 _ -> error $ B.unpack $ B.append "When using a TagRep it must be exactly one tag, you gave: " x

counterOptions :: ParsingOptions CounterBS
counterOptions = ParsingOptions
  { scopeBegin = ""
  , scopeEnd = ""
  , partitionEdge = ""
  , patternBegin = "<div class=\"listing__found\">"
  , patternEnd = "</div>"
  }

instance Parseable CounterBS AdCounter where
  toExtractedObject ctags =
    let extractCounter :: CounterBS -> AdCounter
        extractCounter x = AC (readMaybe $ B.unpack $ B.filter isDigit x)
        pb = patternBegin counterOptions
        pe = patternEnd counterOptions
    in extractCounter $ innerText $ extractTags pb pe ctags
  opts = counterOptions 

instance HttpClient PriceBS AdPrice

instance HttpResponse AdPrice where
  getResponse Nothing = liftIO $ do
    return $ AP 0
  getResponse (Just x) = liftIO $ do
    let ctags = getTags $ encodeUtf8 x
        res = mean priceOptions ctags
    return res

priceOptions :: ParsingOptions CounterBS
priceOptions = ParsingOptions
  { scopeBegin = "<div class=\"listing__results\">"
  , scopeEnd = "<footer data-bem=\"{&quot;footer&quot;:{}}\" class=\"footer i-bem footer_js_inited\">"
  , partitionEdge = "<div class=\"listing-item__details\">"
  , patternBegin = "<div class=\"listing-item__info\">"
  , patternEnd = "<span class=\"i-font i-font_face_rub-arial-regular\">"
  }

instance Parseable PriceBS AdPrice where
  toExtractedObject ctags = AP price
    where tags = extractTags patternBegin' patternEnd' ctags
          pre' = B.filter isDigit $ innerText tags
          price = case pre' of
            "" -> 0
            _  -> read (B.unpack pre') :: Int
          patternBegin' = patternBegin priceOptions
          patternEnd'   = patternEnd priceOptions

  toExtractedList ops ts = b
    where tags = extractTags patternBegin' patternEnd' ts
          e = partitions (~== part) tags
          b = fmap toExtractedObject e

          part = partitionEdge ops
          patternBegin' = scopeBegin ops
          patternEnd'   = scopeEnd ops

  opts = priceOptions

-- TODO
{-
++ По URL1 и числу объявлений получить список URL2
++ По URL2 получить список цен
++ По URL1 и числу объявлений получить среднюю цену
++ По списку со списком цен получить среднюю цену
-- По средней цене получить рекомендацию
-}

getAllPages :: AutoRequest -> AdCounter -> [AutoRequest]
getAllPages ar (AC a) =
  if a < 41 
  then []
  else let pageCount = quot (pred a) 40
       in fmap (setOffset ar)
          $ fmap ((* 40) . succ) [1..pageCount]

setOffset :: AutoRequest -> Offset -> AutoRequest
setOffset (R2 r ma mo y g _ ign) o = R2 r ma mo y g o ign

data M = M !Double !Int
eM :: M
eM = M 0 0
sum' :: M -> AdPrice -> M
sum' !(M x y) !(AP z) = M (x+(fromIntegral z)) (y+1)

total :: [AdPrice] -> M
total x = foldl' sum' eM x

mean ::
  forall a.
  Parseable a AdPrice =>
  ParsingOptions a -> [Tag a] -> AdPrice
mean ops tags = AP (mean2 t)
  where t = total $ toExtractedList ops tags

mean2 :: M -> Int
mean2 (M m n) = floor $ m / (fromIntegral n)

totalMean :: Sess.Session -> AutoRequest -> IO Int
totalMean s autoRequest = do
  extAutoResponse <- performCustomRequest s autoRequest
  autoResponse <- liftIO $ getCustomResponse extAutoResponse
  let tags = getTags autoResponse
      cntr = toExtractedObject tags
      cntr :: AdCounter
      firstPrice = mean priceOptions tags
      firstPrice :: AdPrice
      reqs = getAllPages autoRequest cntr
  resps <- liftIO $! mapM (\x -> wait1 >> httpAction s x (getExtendedOptions x extAutoResponse)) reqs
  putStrLn $ T.pack $ "Total: " ++ (show $ ac cntr)
  putStrLn $ T.pack $ "Length of " ++ (show $ length reqs)
  let newTagList = fmap getTags resps
      newTagList :: [[Tag B.ByteString]]
      prices = filter ((/= 0) . apr) $ fmap (mean priceOptions) newTagList
      prices :: [AdPrice]
      mprice = mean2 . total $ firstPrice : prices
  return mprice

{- on each page do:
* define images and load them
* define JS and load them
* send query to awsync.yandex.ru/...
* update cookies after each request/response.
-}

readMaybe :: String -> Int
readMaybe "" = 0
readMaybe a = read a :: Int
--readMaybe _  = 0

adCheckAction :: IO ()
adCheckAction = do
  t <- readFile "adlist.txt"
  let l = B.lines t
  B.writeFile "adcounterout.txt" ""
  s <- Sess.withSession $ \sess -> return sess
  mapM_ (runTest s) l

runTest :: Sess.Session -> CounterBS -> IO ()
runTest s l = do
  r <- httpAction s l (getOptions l)
  B.appendFile "adcounterout.txt" $ B.concat [ l, ": " , B.pack . show $ ac r, "\n"]

adPriceCheckAction :: IO ()
adPriceCheckAction = do
  t <- readFile "adlist2.txt"
  let l = B.lines t
  B.writeFile "adcpriceout.txt" ""
  s <- Sess.withSession $ \sess -> return sess
  mapM_ (runPriceTest2 s) l

runPriceTest2 :: Sess.Session -> B.ByteString -> IO ()
runPriceTest2 s l = do
  let u = R2 (T.pack $ B.unpack l) "" "" "" "" 0 True
      u :: AutoRequest
  r <- totalMean s u
  B.appendFile "adcpriceout.txt" $ B.concat [ l, ": " , B.pack . show $ r, "\n"]

