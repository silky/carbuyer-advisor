{-# LANGUAGE OverloadedStrings #-}
module Handler.Types where

import Import
import Form
import qualified Data.List as L 
import Database.Persist.Sql

getTypesR :: Handler Html
getTypesR = do
  allents <- runDB $ rawSql tquery []
  gtypes <- mapM summary allents
  let ggtypes = L.groupBy grouping $ fmap shiftSummary gtypes

      
  (typeWidget, enctype) <- generateFormPost $ bootstrapTypeForm
  defaultLayout $ do
      addStylesheet $ StaticR css_bootstrap_css
      setTitle "Administration"
      $(widgetFile "types")
      $(fayFile "Types")  

postTypesR :: Handler Html
postTypesR = do
    ((res,typeWidget),enctype) <- runFormPost $ bootstrapTypeForm
    case res of
         FormSuccess typ -> do
            typeId <- runDB $ insert typ
            redirect $ TypeR typeId
         _ -> defaultLayout $ do
                addStylesheet $ StaticR css_bootstrap_css
                setTitle "Please correct your entry form"
                $(widgetFile "typeAddError")

