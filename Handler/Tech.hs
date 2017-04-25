{-# LANGUAGE OverloadedStrings #-}
module Handler.Tech where

import Cache
import Import hiding (head, length, pack)
import Database.Persist.Sql
import Auto.JSONTools

import Data.List (head, length)
import Data.Text (pack)
import Form
import SessionState

import qualified Text.JSON as J

getTechR :: Handler Html
getTechR = renderError ("Заполните форму" :: Text)

postTechR :: Handler Html
postTechR = do
  uf <- getUrlRender
  let url = uf (TechR)
  adminPass TechE url (continueTechResult)

continueTechResult = do
  app <- getYesod
  c <- getCache
  let marks = getMarks c
  result <- runInputPostResult itemForm
  case result of
    FormSuccess (Response' req) -> do
      liftIO $ logMessageSite app req
      resp <- decodeRequest req
      let res = validate resp
      case res of
        Left msg   -> renderError msg
        Right (R2 _) -> redirect TechR
        Right r@(R ma mo ye gen) -> do
           liftIO $ logMessageSite app $ "Parsing results: " <> (pack . show $ r)
           liftIO $ logMessageSite app ("Result was parsed successfully." :: Text)
           let cgens = filter (byKey mo . generationModelId . entityVal) $ getGenerations c
           liftIO $ logMessageSite app $ "Current generation list: " <> (pack . show . fmap entityKey $ cgens) <> "model_id: " <> (pack . show $ fromIntegral mo)
           let gens = getGenerations c
               marks = getMarks c
               models = getModels c
               years = getAges c
               gs = case gen of
                 0 -> let currentAge = head
                            $ fmap (ageAge . entityVal)
                            $ filter (byKey ye . entityKey)
                            $ years
                          currentGens = filter (byKey mo . generationModelId . entityVal) gens
                      in filter ((<= (Just currentAge)) . generationBottomAge . entityVal)
                         $ filter ((>= (Just currentAge)) . generationTopAge . entityVal)
                         currentGens
                 _ -> filter (byKey gen . entityKey) gens
               gs :: [Entity Generation]
               ma' = head $ filter (byKey ma . entityKey) marks
               mo' = head $ filter (byKey mo . entityKey) models
               ye' = head $ filter (byKey ye. entityKey) years
               ge = head gs
           liftIO $ logMessageSite app $ "Resulting generation list: " <> (pack . show . fmap entityKey $ gs)
           advices <- runDB $ do
             tags <- selectList [ LkTagGeneration <-. (fmap entityKey gs) ] []
             selectList [ TextAdviseId <-. (fmap (lkTagTextAdviseId . entityVal) tags)
                        ] [Desc TextAdviseId]
           if null advices
             then renderOk (ma', mo', ye',ge)
             else redirect $ TechWithIdR (entityKey $ head advices)
    _ -> redirect $ TechR


type ValidateResult = Either ErrorMessage Result
type ErrorMessage = Text
data Result = R Int Int Int Int | R2 Init  deriving Show
data Init = IR Text Text Text Text deriving Show
data Response' = Response' String deriving Show

itemForm :: forall (m :: * -> *).
                  (Monad m, RenderMessage (HandlerSite m) FormMessage) =>
                  FormInput m Response'
itemForm = Response'
    <$> ireq hiddenField ("tech-q" :: Text)

instance J.JSON Response' where
  showJSON = error "unable to show JSON"
  readJSON (J.JSObject o) = Response' <$> o ! "instance"
  readJSON _ = mzero

instance J.JSON Init where
  showJSON = error "unable to show JSON"
  readJSON (J.JSObject o) =
    IR <$> o ! "slot1"
       <*> o ! "slot2"
       <*> o ! "slot3"
       <*> o ! "slot4"
  readJSON _ = mzero      

decodeRequest ::
  forall (m :: * -> *).
  (MonadHandler m, MonadBaseControl IO m, HandlerSite m ~ App) =>
  String -> m ValidateResult
decodeRequest req = return $ safeFromResult (J.decode req :: J.Result Init)

safeFromResult :: J.Result Init -> ValidateResult
safeFromResult (J.Ok a) = Right (R2 a)
safeFromResult (J.Error _) = Left "Некоторые параметры были не заполнены"

validate :: ValidateResult -> ValidateResult
validate (Right (R2 (IR ma' mo' y' ge'))) =
  if isInt ge' && ge' /= "0"
  then Right (R 0 0 0 (parseInt ge'))
  else
    if (and $ fmap isInt [ma', mo', y'])
    then Right (R (parseInt ma') (parseInt mo') (parseInt y') 0)
    else Left "Некоторые параметры были заполнены неверно"
validate (Left a) = Left a
validate (Right r@(R _ _ _ _)) = Right r



renderError str = do
  c <- getCache
  let marks = getMarks c
  let emsg = str
  defaultLayout $ do
    addStylesheet $ StaticR css_bootstrap_css
    setTitle "Ошибка"
    $(widgetFile "tech-error")
    $(fayFile "TechErrorF")  

renderOk :: (Entity Mark, Entity Model, Entity Age, Entity Generation)
  -> HandlerT App IO Html
renderOk (ma',mo',ye',ge') = do
  c <- getCache
  let marks = getMarks c
      ma = ma'
      mo = mo'
      g = ge'
      a = ye'

  defaultLayout $ do
    addStylesheet $ StaticR css_bootstrap_css
    setTitle "Запрос техсправки"
    $(widgetFile "tech-request")
    $(fayFile "TechRequestF")
