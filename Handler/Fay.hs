{-# LANGUAGE OverloadedStrings #-}
module Handler.Fay where

import Fay.Convert (readFromFay)
import Data.List (nub, head)
import Import hiding (pack, head, concat)
import Yesod.Fay
import Database.Persist.Sql
import Data.Text (pack, concat)
import Cache
import Data.ByteString.Base64 (decodeLenient)
import System.Random
import Control.Concurrent (threadDelay)
import Form
import SessionState

onCommand :: CommandHandler App
onCommand render command = do
    c <- getCache
    case readFromFay command of
      Just (AModel Nothing r) -> render r []
      Just (AModel (Just i) r) -> do
        let k = toSqlKey $ fromIntegral i
            m = filter ((==k) . modelMarkId . entityVal) $ getModels c
        render r $ fmap modelToFront m
      Just (AAge Nothing r) -> render r []
      Just (AAge (Just i) r) -> do
        let k = toSqlKey $ fromIntegral i
            m = nub
                $ fmap (lkModelAgeAgeId . entityVal)
                $ filter ((==k) . lkModelAgeModelId . entityVal) $ getLkModelAges c
            a :: [Entity Age]
            a = filter (\x -> elem (entityKey $ x) m) $ getAges c
        render r $ fmap ageToFront a
      Just (AGen (m,a) r) -> do
        case or $ fmap (==Nothing) [m,a] of
          True  -> render r []
          False ->  do
            let (Just km) = fmap (toSqlKey . fromIntegral) m
                (Just ka) = fmap (toSqlKey . fromIntegral) a
                lma = nub . filter ((==km) . lkModelAgeModelId . entityVal)
                      . filter ((==ka) . lkModelAgeAgeId . entityVal)
                      $ getLkModelAges c
                stat = case null lma of
                  True  -> []
                  False -> let stat2 = lkModelAgeMultiple . entityVal . head $ lma
                               age = ageAge . entityVal $ head
                                     $ filter ((==ka) . entityKey) $ getAges c
                               gen2  = filter ((>= age) . fromJustInt . generationTopAge . entityVal) 
                                       . filter ((<= age) . fromJustInt . generationBottomAge . entityVal)
                                       $ gen
                           in if (stat2 == False)
                              then []
                              else (nub $ fmap generationToFront gen2)
                gen = filter ((==km) . generationModelId . entityVal) $ getGenerations c
            render r stat
      Just (LkAdd (Nothing,_) r) -> render r Nothing
      Just (LkAdd (_,Nothing) r) -> render r Nothing
      Just (LkAdd (Just i,Just ta) r) -> do
        let k = toSqlKey $ fromIntegral i
            (g:_) = filter ((==k) . entityKey) $ getGenerations c
            tak = toSqlKey $ fromIntegral ta
        _ <- runDB $ insertUnique $ LkTag tak k
        (ma,mo,ge) <- runDB $ do
          (ge':_) <- selectList [GenerationId ==. k] [LimitTo 1]
          (mo':_) <- selectList [ModelId ==. (generationModelId . entityVal $ ge')] [LimitTo 1]
          (ma':_) <- selectList [MarkId  ==. (generationMarkId  . entityVal $ ge')] [LimitTo 1]
          return (ma',mo',ge')
        runDB $ update tak [TextAdviseName =. (showQueryGen ma mo ge)]  
        render r $ Just ( pack $ show i
                        , fromJustText . generationGeneration . entityVal $ g
                        , textFromJustInt . generationBottomAge . entityVal $ g
                        , textFromJustInt . generationTopAge . entityVal $ g)
      Just (LkDel (Nothing,_) r) -> render r False
      Just (LkDel (_,Nothing) r) -> render r False
      Just (LkDel (Just i, Just ta) r) -> do
        runDB $ deleteWhere [ LkTagTextAdviseId ==. (toSqlKey $ fromIntegral ta)
                            , LkTagGeneration ==. (toSqlKey $ fromIntegral i)
                            ]
        render r True
      Just (ImgAdd (Nothing,_) r) -> render r []
      Just (ImgAdd (_,[]) r) -> render r []
      Just (ImgAdd (Just i,is) r) -> do
        let files = fmap processFileFromText is
        fnames <- liftIO $ mapM writeToServer files
        let images = fmap (flip toImage (toSqlKey $ fromIntegral i)) fnames
        runDB $ mapM_ insert_ images
        render r $ fmap imgToFront images
      Just (ImgDel Nothing r) -> render r False
      Just (ImgDel (Just i) r) -> do
        runDB $ deleteWhere [ ImageId ==. (toSqlKey $ fromIntegral i)
                            ]
        render r True
      Just (ReqAdd (IR3 ge em) r) -> do
        let gen = selectGeneration c ge
        runDB $ insert_ $ NewAdvice em gen
        render r True
      Just (PAdd p@(PRInsert k v d) r) -> do
        p' <- runDB $ selectList [AsPropertyK ==. k] [LimitTo 1]
        if null p'
          then do
            _ <- runDB $ insertUnique $ AsProperty k v d
            render r $ Just p
          else render r Nothing
      Just (PriceMatrix prices r) -> do
        let validatedPrices = filter validated $ fmap validate prices
            insertPrice (PriceV v s h _) = runDB $ do
              k <- insertObj v 3
              insertRef 1 k (toSqlKey . fromIntegral $ s)
              insertRef 2 k (toSqlKey . fromIntegral $ h)
            insertRef atr obj ref = insert_ $ AsParam (toSqlKey atr) obj Nothing Nothing (Just ref)
            insertObj nam typ = insert $ AsObject (pack . show $ nam) (toSqlKey typ) Nothing Nothing 0
        logMessage $ pack . show $ length $ validatedPrices
        runDB $ do
          ps <- selectList [AsObjectT ==. (toSqlKey 3)] []
          let kps = fmap entityKey ps
          deleteWhere [AsParamAo <-. kps]
          deleteWhere [AsParamAr <-. (fmap return kps)]
          deleteWhere [AsObjectId <-. kps]
        mapM_ insertPrice validatedPrices
        render r True
      Nothing               -> invalidArgs ["Invalid command"]



fromJustInt :: Maybe Int -> Int
fromJustInt Nothing = 1900
fromJustInt (Just a) = a

fromJustText :: Maybe Text -> Text
fromJustText Nothing = "I"
fromJustText (Just a) = a 

textFromJustInt :: Maybe Int -> Text
textFromJustInt Nothing = "-"
textFromJustInt (Just a) = pack . show $ a

processFileFromText :: Text -> ByteString
processFileFromText = decodeLenient . encodeUtf8 . drop 1 . dropWhile (/= ',') 

writeToServer :: ByteString -> IO Text
writeToServer content = do
  name <- randomName
  let fname = "static/img/" <> name <> ".png"
  writeFile (unpack fname) content
  return $ "/" <> fname

toImage :: Text -> Key TextAdvise -> Image
toImage url ta = Image url ta

imgToFront :: Image -> (Text,Text)
imgToFront (Image url gen) = (pack . show . fromSqlKey $ gen, url)

randomName :: IO Text
randomName = do
  threadDelay 1000000
  t <- getCurrentTime
  return $ pack . take 10 $ randomRs ('a','z') $
    mkStdGen (parseInt $ pack $ formatTime defaultTimeLocale "%s" t)

chooseGeneration :: Text -> Text -> Text -> HandlerT App IO (Key Generation)
chooseGeneration g y m = do
  c <- getCache
  let models = filter (byKey mo . entityKey) $ getModels c
      gens = getGenerations c
      ages = getAges c
      ge = parseInt g
      ye = parseInt y
      mo = parseInt m
      gs = case ge of
             0 -> let currentAge = head
                       $ fmap (ageAge . entityVal)
                       $ filter (byKey ye . entityKey)
                       $ ages
                      currentGens = filter (byKey mo . generationModelId . entityVal) gens
                  in filter ((<= (Just currentAge)) . generationBottomAge . entityVal)
                      $ filter ((>= (Just currentAge)) . generationTopAge . entityVal)
                     currentGens
             _ -> filter (byKey ge . entityKey) gens
  return $ entityKey $ head gs
    
selectGeneration c ge = entityKey $ head $ filter (byKey (parseInt ge) . entityKey) (getGenerations c)

validate :: Price -> PriceV
validate (Price v s h) =
  if and [isMaybe s, isMaybe h, isDouble v]
  then PriceV (parseDouble v) (safeFromJust 0 s) (safeFromJust 0 h) True
  else PriceV 0.0 0 0 False
