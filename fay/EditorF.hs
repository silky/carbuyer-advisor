{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module EditorF where

import           FFIExample

import           DOM
import           Data.Text (fromString)
import qualified Data.Text as T
import           Fay.Yesod
import           Prelude
import           SharedTypes

main :: Fay ()
main = do
    add <- getElementById "padd"

    onClick add $ do
        k <- getInputVal "pk"
        v <- getInputVal "pv"
        d <- getInputVal "pd"
        let val = PRInsert k v d
        call (PAdd val) $ addP "ptable"
      
addP :: T.Text -> Maybe PropertyRequest -> Fay ()
addP tab p = case p of
  Nothing -> return ()
  Just pid -> createRow tab pid

createRow :: T.Text -> PropertyRequest -> Fay ()
createRow tab (PRInsert k v d) = do
  row <- createElement "tr"
  table <- getElementById tab
  appendChild table row
  setAttr row "class" "row-md-12"
  mapM_ (createCell row)
    [ (k, "col-md-6")
    , (v, "col-md-6")
    ]

createCell :: Element -> (T.Text,T.Text) -> Fay ()
createCell row (val,cval) = do
  cell <- createElement "td"
  appendChild row cell
  setInnerHTML cell val
  setAttr cell "class" cval
