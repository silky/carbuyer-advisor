{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Example of defining FFI functions.
--
-- The `ffi' method is currently incompatible with 'RebindableSyntax',
-- so these are defined in another module.

module FFIExample where

import Data.Text (Text, pack, concat)
import DOM
import FFI

onKeyUp :: Element -> Fay () -> Fay ()
onKeyUp = ffi "%1.onkeyup=%2"

onKeyPress :: Element -> Fay () -> Fay ()
onKeyPress = ffi "%1.onkeypress=%2"

onKeyDown  :: Element -> Fay () -> Fay ()
onKeyDown = ffi "%1.onkeydown=%2"

setInnerHTML :: Element -> Text -> Fay ()
setInnerHTML = ffi "%1.innerHTML=%2"

onClick :: Element -> Fay () -> Fay ()
onClick = ffi "%1.onclick=%2"

onSubmit :: Element -> Fay () -> Fay ()
onSubmit = ffi "%1.onsubmit=%2"

alert :: Text -> Fay ()
alert = ffi "alert(%1)"

getValById :: Text -> Fay Text
getValById = ffi "document.getElementById(%1).innerHTML"

ajaxDelete :: Text -> Fay ()
ajaxDelete = ffi "jQuery['ajax']({ url: window.location.href, type: 'DELETE', success: function (html) { window.location.replace(window.location.href.replace(%1, '')) }})"

onChange :: Element -> Fay () -> Fay ()
onChange = ffi "%1.onchange=%2"

log' :: a -> Fay ()
log' = ffi "console.log(%1)"

onSelect :: Element -> Fay () -> Fay ()
onSelect = ffi "%1.onselect=%2"

onFocus :: Element -> Fay () -> Fay ()
onFocus = ffi "%1.onfocus=%2"

onMouseOver :: Element -> Fay () -> Fay ()
onMouseOver = ffi "%1.onmouseover=%2"

sort :: forall a. [a] -> Fay [a]
sort = ffi "%1.sort()"

getComboVal :: Text -> Fay Text
getComboVal eid = do
  ival <- getInputValue eid
  getDataListValue eid ival

parseMaybeInt :: Text -> Fay (Maybe Int)
parseMaybeInt t = case t of
  ""  -> return Nothing
  _   -> do
    v <- parseInt t
    return $ Just v

getIntVal :: Text -> Fay (Maybe Int)
getIntVal eid = do
  ival <- getInputValue eid
  val <- getDataListValue eid ival
  parseMaybeInt val

parseJVal :: Text -> Fay (Maybe Int)
parseJVal a = do
  b <- getJValById a
  parseMaybeInt b

getUrl :: Text -> Fay Text
getUrl = ffi "window.location.href.replace(%1, '')"

getJValById :: Text -> Fay Text
getJValById = ffi "$('#'.concat(%1)).val()"

getOptionText :: Text -> Fay Text
getOptionText = ffi "$('#'.concat(%1)).find(':selected').text()"

getOptionVal :: Text -> Fay Text
getOptionVal = ffi "$('#'.concat(%1)).find(':selected').val()"

setProgress :: Text -> Text -> Fay ()
setProgress = ffi "$('#'.concat(%1)).attr('style', 'width:'.concat(%2)).text(%2)"

hideElem :: Text -> Fay ()
hideElem elem = toggleElemClassByName elem "hide"

showElem :: Text -> Fay ()
showElem elem = toggleElemClassByName elem "show"

toggleCurrElem :: Text -> Text -> Fay ()
toggleCurrElem eid tgl = do
  elem <- getElementById eid
  toggleClass' elem tgl ["show", "hide"]

toggleElemClassByName :: Text -> Text -> Fay ()
toggleElemClassByName elemName tgl = do
   (el:_) <- getElementsByName elemName
   p <- parentNode el
   toggleClass' p tgl ["show", "hide"]

toggleClass' :: Element -> Text -> [Text] -> Fay ()
toggleClass' elem tgl cs = do
   mapM_ (rmClass elem) cs
   addClass elem tgl

toggleClassById :: Text -> Text -> [Text] -> Fay ()
toggleClassById eid tgl cs = do
  elem <- getElementById eid
  toggleClass' elem tgl cs

rmClass :: Element -> Text -> Fay ()
rmClass elem cl = do
  b <- hasClass elem cl
  removeClass elem cl
  {-case b of
    True  -> removeClass elem cl
    False -> return ()-}

rmAttr :: Element -> Text -> Fay ()
rmAttr = ffi "%1.removeAttribute(%2)"

rmAttrById :: Text -> Text -> Fay ()
rmAttrById eid attr = do
  elem <- getElementById eid
  rmAttr elem attr

setAttrById :: Text -> Text -> Text -> Fay ()
setAttrById = ffi "document['getElementById'](%1).setAttribute(%2, %3)"

enableById :: Text -> Fay ()
enableById = ffi "document.getElementById(%1).removeAttribute('disabled')"

disableById :: Text -> Fay ()
disableById eid = setAttrById eid "disabled" "disabled"

setInner :: Element -> Text -> Fay ()
setInner = ffi "%1.innerHTML=%2"

getInner :: Element -> Fay Text
getInner = ffi "%1.innerHTML"

setBgColor :: Element -> Text -> Fay ()
setBgColor = ffi "%1.style='width:%%;'"

removeClassById :: Text -> Text -> Fay ()
removeClassById = ffi "$('#' + %1).removeClass(%2)"

replaceTwoElementsById :: Text -> Text -> Fay ()
replaceTwoElementsById shown hidden = do
  removeClassById hidden "hide"
  toggleCurrElem hidden "show"
  mapM_ (removeClassById shown) [ "show", "hide" ]
  toggleCurrElem shown "hide"

submitFormById :: Text -> Fay ()
submitFormById = ffi "document.getElementById(%1).submit()"

getInputValue :: Text -> Fay Text
getInputValue = ffi "$('#' + %1 + '-input').val()"

getInputVal :: Text -> Fay Text 
getInputVal = ffi "$('#'.concat(%1)).val()"

getDataListValue :: Text -> Text -> Fay Text
getDataListValue = ffi "$('#' + %1 + ' option[value=\"' + %2 + '\"]').attr('id')"

getJChildren :: Text -> Fay [Element]
getJChildren = ffi "$('#' + %1).children()"

refreshComboBox :: Text -> Fay ()
refreshComboBox = ffi "$('#' + %1).data('combobox').refresh()"

addComboBox :: Fay ()
addComboBox = ffi "$('.combobox').combobox({appendId: \"-input\", bsVersion: '3', menu: '<ul class=\"typeahead typeahead-long dropdown-menu\" style=\"left:0;\"></ul>'})"

onComboBoxClick :: Text -> Fay ()
onComboBoxClick = ffi "$('#' + %1).parent().children().find('.caret').click()"

clickInputCombo :: Text -> Fay ()
clickInputCombo eid = do
  elem <- getElementById eid
  onClick elem $ onComboBoxClick eid 

comboValToNull :: Text -> Text -> Fay ()
comboValToNull = ffi "$('#' + %1).val($('#' + %2).children()[0].value)"

getElementsByClass :: Text -> Fay [Element]
getElementsByClass = ffi "document.getElementsByClassName(%1)"

