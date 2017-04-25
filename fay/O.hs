{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module TextAdvise where

import           FFIExample

import           DOM
import           Data.Text (fromString)
import qualified Data.Text as T
import           Fay.Yesod
import           Prelude
import           SharedTypes

main :: Fay ()
main = do
    btn <- getElementById "sbmt"

    onClick btn $ do
      input <- getElementById "ps"
      rows <- getCells input
      prices <- mapM getPrices rows
      call (PriceMatrix prices) $ emptyAction

getCells :: Element -> Fay [Element]
getCells elem = getElementsByClass "tdinp"

emptyAction :: Bool -> Fay ()
emptyAction _ = return ()

getChildren :: Element -> Fay [Element]
getChildren elem = do
  nodes <- childNodes elem
  return $ nodeListToArray nodes

getManyChildren :: [Element] -> Fay [Element]
getManyChildren elems = do
  c1 <- mapM getChildren elems
  return $ concat c1

getPrices :: Element -> Fay Price
getPrices elem = do
  serv <- getMaybeIntFromAttr elem "serv"
  hyp  <- getMaybeIntFromAttr elem "hyp"
  val <- getValue elem
  return $ Price val serv hyp
  

getMaybeIntFromAttr :: Element -> Text -> Fay (Maybe Int)
getMaybeIntFromAttr elem attr = do
  val <- getAttr elem attr
  parseMaybeInt val
