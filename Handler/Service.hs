{-# LANGUAGE OverloadedStrings #-}
module Handler.Service where

import Import hiding (head)
import Database.Persist
import Database.Persist.Sql
import SessionState
import Data.List (nub, head)

import qualified Database.Esqueleto as E

getServiceR :: Handler Html
getServiceR = do
  services <- getByType (toSqlKey 1)
  hyps     <- getByType (toSqlKey 2)
  existedPrices <- runDB $ selectList [AsObjectT ==. (toSqlKey 3)] []
  allInfo <- runDB $ getPrices
  let prices = if null existedPrices
               then setPrices services hyps
               else showPrices allInfo

  defaultLayout $ do
    addStylesheet $ StaticR css_bootstrap_css
    setTitle "Услуги и гипотезы"
    $(widgetFile "o")
    $(fayFile "O")

getByType :: forall site. (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => Key AsType -> HandlerT site IO [Entity AsObject]
getByType typ = runDB $ selectList [AsObjectT ==. typ] [Asc AsObjectId]

setPrices :: [Entity AsObject] -> [Entity AsObject] -> PriceTable
setPrices services hyps =
  let mult = [Price' 0.0 x y | x <- services',  y <- hyps']
      services' = getKeys services
      prices = groupBy compServices $ sortBy (compare `on` serv') mult
      compServices a b = serv' a == serv' b
      snames = getNames services
      hyps' = getKeys hyps
  in zip prices snames

{-getPrices = runDB $ rawSql "select o.n, rs.ar, rh.ar, s.n from as_object o, as_param rs, as_param rh, as_object s where o.t = 3 and o.id = rs.ao and rs.aa = 1 and o.id = rh.ao and rh.aa = 2 and rs.ar = r.id order by s.id, rh.ar" []-}
getPrices = E.select $
  E.from $ \(o `E.InnerJoin` rs `E.InnerJoin` rh `E.InnerJoin` s) -> do
  E.on (rs E.^. AsParamAr E.==. s E.?. AsObjectId)
  E.on ( o E.^. AsObjectId E.==. rh E.^. AsParamAo
         E.&&. rh E.^. AsParamAa E.==. (E.valkey 2)
       )
  E.on ( o E.^. AsObjectId E.==. rs E.^. AsParamAo
         E.&&. rs E.^. AsParamAa E.==. (E.valkey 1)
       )
  return $ (o E.^. AsObjectN, rs E.^. AsParamAr, rh E.^. AsParamAr, s E.?. AsObjectN)

getKeys = fmap entityKey
getNames = fmap getName
getName = asObjectN . entityVal
getRef = safeFromJust (toSqlKey 0) . asParamAr . entityVal
showPrices = fmap cell2Table . groupBy compSnd . sortBy (compare `on` (snd)) . fmap showPrice
  where compSnd a b = snd a == snd b
showPrice (p, rs, rh, s) =
  (Price' (parseDouble $ E.unValue p) (safeNum rs) (safeNum rh), safeText s)
  where safeNum = safeFromJust (toSqlKey 0) . E.unValue
        safeText = safeFromJust "" . E.unValue

cell2Table :: [PriceCell] -> PriceRow
cell2Table a =
  let b2 = head $ nub $ fmap snd a
      b1 = fmap fst a
  in (b1, b2)

data Price' =
  Price' { val' :: Double
        , serv' :: AsObjectId
        , hyp' :: AsObjectId
        }


type PriceRow = ([Price'], ServiceName)
type PriceCell = (Price', ServiceName)
type PriceTable = [PriceRow]
type ServiceName = Text
