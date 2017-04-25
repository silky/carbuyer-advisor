{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module RegionF where

import           FFIExample

import           DOM
import           Data.Text (fromString)
import qualified Data.Text as T
import           Fay.Yesod
import           Prelude hiding (sort)
import           FFI


main :: Fay ()
main = do
  btn <- getElementById "region-submit"
  els <- getElementsByName "region-input"

  onClick btn $ sendRequest els

data Request = Request { rid :: Int
                       , rvalue :: T.Text
                       } 

toRequest :: (Int,T.Text) -> Fay Request
toRequest (i,v) = return Request {rid = i, rvalue = v}


getRequest :: [Element] -> Fay [Request]
getRequest els = do
  ids  <- mapM (\x -> getAttr x "id") els
  iids <- mapM parseInt' ids
  vals <- mapM getJValById ids
  r <- mapM toRequest $ zip iids vals
  return r

sendRequest :: [Element] -> Fay ()
sendRequest d = do
  req <- getRequest d
  js' <- toJSON req
  fr <- getElementById "regionForm"
  setAttr fr "value" js'
  b  <- getElementById "regionF"
  submitForm b
  
toJSON :: [Request] -> Fay T.Text
toJSON = ffi "JSON.stringify(%1)"

submitForm :: Element -> Fay ()
submitForm = ffi "%1.submit()"

parseInt' :: T.Text -> Fay Int
parseInt' = ffi "parseInt(%1.replace('','0'))"
