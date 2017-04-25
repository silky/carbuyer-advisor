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
    input <- getElementById "del"
    val   <- getValById "textadvise"
    let url = T.concat [ "/text-advise" , "/" , val ]
    onClick input $ 
        ajaxDelete url


    taInt <- parseMaybeInt val
    addBtn <- getElementById "lkadd"

    onClick addBtn $ do
      gen <- getOptionVal "genupd"
      genInt <- parseMaybeInt gen
      call (LkAdd (genInt, taInt)) $ addGen "lkaddtable"

    delBtn <- getElementsByClass "lkdel"
    
    mapM_ (\x -> onClick x $ do
      cell <- parentNode x
      row <- parentNode cell
      ival <- getAttr x "data"
      iival <- parseMaybeInt ival
      call (LkDel (iival,taInt)) $ delRow row) delBtn

    imgBtn <- getElementById "imgadd"

    onClick imgBtn $ do
      imgs <- getElementsByClass "generated"
      imgData <- mapM (\x -> getAttr x "src") imgs
      call (ImgAdd (taInt, imgData)) $ addImg "imgaddtable"

    imgDelBtn <- getElementsByClass "imgdel"
    
    mapM_ (\x -> onClick x $ do
      cell <- parentNode x
      row <- parentNode cell
      ival <- getAttr x "data"
      iival <- parseMaybeInt ival
      call (ImgDel iival) $ delRow row) imgDelBtn

addGen :: T.Text -> Maybe (T.Text, T.Text, T.Text, T.Text) -> Fay ()
addGen tab g = case g of
  Nothing -> return ()
  Just gid -> createRow "link" tab gid

createRow :: T.Text -> T.Text -> (T.Text, T.Text, T.Text, T.Text) -> Fay ()
createRow "link" tab (gid, gname, gba, gta) = do
  row <- createElement "tr"
  table <- getElementById tab
  appendChild table row
  setAttr row "class" "row"
  createButtonCell row gid "Удалить"
  mapM_ (createCell row)
    [ (gname, "col-md-5")
    , (gba, "col-md-3")
    , (gta, "col-md-3")
    ]
createRow "image" tab (gid, imgurl, _, _) = do
  row <- createElement "tr"
  table <- getElementById tab
  appendChild table row
  setAttr row "class" "row"
  createButtonCell row gid "Удалить"
  createImgCell row imgurl

createCell :: Element -> (T.Text,T.Text) -> Fay ()
createCell row (val,cval) = do
  cell <- createElement "td"
  appendChild row cell
  setInnerHTML cell val
  setAttr cell "class" cval

createImgCell :: Element -> T.Text -> Fay ()
createImgCell row url = do
  cell <- createElement "td"
  appendChild row cell
  img <- createElement "img"
  appendChild cell img
  setAttr img "src" url
  setAttr img "class" "thumb"

createButtonCell :: Element -> T.Text -> T.Text -> Fay ()
createButtonCell row gid val = do
  cell <- createElement "td"
  appendChild row cell
  setAttr cell "class" "col-md-1"

  btn <- createElement "button"
  appendChild cell btn
  setValue btn val
  setInnerHTML btn val
  setAttr btn "class" "btn lkdel"
  setAttr btn "data" gid

delRow :: Element -> Bool -> Fay ()
delRow _ False = return ()
delRow row True = do
  table <- parentNode row
  removeChild table row


addImg :: T.Text -> [(T.Text, T.Text)] -> Fay ()
addImg tab g = do
  g4 <- mapM t2t4 g
  mapM_ (createRow "image" tab) g4

t2t4 :: (T.Text, T.Text) -> Fay (T.Text, T.Text, T.Text, T.Text)
t2t4 (a,b) = return (a,b,"","")
