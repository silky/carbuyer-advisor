{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Widget where

import           FFIExample

import           DOM
import           Data.Text (fromString)
import qualified Data.Text as T
import           Fay.Yesod
--import           Fay.Text (toJSON)
import           Prelude hiding (sort)
import           FFI

import SharedTypes


data CallType = CallEmpty () | CallMaybe (Maybe Int) | CallMaybeTwice (Maybe Int, Maybe Int)

data Entity =
  Entity { selectId :: T.Text
         , inputId  :: T.Text
         , flushComboBox :: [(T.Text, T.Text)]
         , customFunc :: Fay ()
         , switchWidgets :: (T.Text, [T.Text])
         , enableWidgets :: [T.Text]
         , disableWidgets :: [T.Text]
         , events :: [(Element -> Fay () -> Fay ())]
         , callFunc :: (T.Text -> Fay (Maybe Int)) -> Fay ()
         , valFunc :: T.Text -> Fay (Maybe Int)
         , hideFunc :: Fay ()
         } 

region :: Entity
region = Entity 
  { selectId = "region"
  , inputId  = "region-input"
  , flushComboBox = [("model","модель"), ("year","год"),("generation","поколение")]
  , customFunc = return ()
  , switchWidgets  = ("mark-input", ["model-input","year-input","generation-input","budget"])
  , enableWidgets  = ["mark", "mark-input"]
  , disableWidgets = ["model-input","year-input","generation-input","budget","form-submit"]
  , events = [onChange, onSelect, onFocus]
  , callFunc = (\_ -> return ())
  , valFunc = parseJVal
  , hideFunc = hideGenerationMessage
  }

mark :: Entity
mark = Entity
  { selectId = "mark"
  , inputId  = "mark-input"
  , flushComboBox = [("model","модель"), ("year","год"),("generation","поколение")]
  , customFunc = return ()
  , switchWidgets  = ("model-input", ["year-input","generation-input","budget"])
  , enableWidgets  = ["model", "model-input"]
  , disableWidgets = ["model-input","year-input","generation-input","budget","form-submit"]
  , events = [onChange, onSelect, onFocus]
  , callFunc = (\f -> do
       val <- f "mark"
       call (AModel val) $ createOptionsByIdWithRefresh "model")
  , valFunc  = parseJVal
  , hideFunc = hideGenerationMessage
  }

model :: Entity
model = Entity
  { selectId = "model"
  , inputId  = "model-input"
  , flushComboBox = [("year","год"),("generation","поколение")]
  , customFunc = return ()
  , switchWidgets  = ("year-input", ["generation-input","budget"])
  , enableWidgets  = ["year", "year-input"]
  , disableWidgets = ["year-input","generation-input","budget","form-submit"]
  , events = [onChange, onSelect, onFocus]
  , callFunc = (\f -> do
        val <- f "model"
        call (AAge val) $ do createOptionsByIdWithRefresh "year")
  , valFunc  = getIntVal
  , hideFunc = hideGenerationMessage
  }

age :: Entity
age = Entity
  { selectId = "year"
  , inputId  = "year-input"
  , flushComboBox = [("generation","поколение")]
  , customFunc = return ()
  , switchWidgets  = ("year-input", ["budget"])
  , enableWidgets  = ["generation", "generation-input", "budget"]
  , disableWidgets = ["generation-input","budget","form-submit"]
  , events = [onChange, onSelect, onFocus]
  , callFunc = (\f -> do
        mval <- f "model"
        val <- f "year"
        call (AGen (mval,val)) $ processMultipleGens "generation")
  , valFunc  = getIntVal
  , hideFunc = restoreBudgetWidget
  }

generation :: Entity
generation = Entity
  { selectId = "generation"
  , inputId  = "generation-input"
  , flushComboBox = []
  , customFunc = return ()
  , switchWidgets  = ("generation", ["budget"])
  , enableWidgets  = []
  , disableWidgets = ["budget","form-submit"]
  , events = [onChange, onSelect, onFocus]
  , callFunc = (\f -> do
                   x <- f "generation"
                   case x of
                     Nothing -> return ()
                     _ -> restoreBudgetWidget)
  , valFunc  = getIntVal
  , hideFunc = return ()
  }

------------------------------------------------------------------------------------

techMark :: Entity
techMark = Entity
  { selectId = "tech-mark"
  , inputId  = "tech-mark-input"
  , flushComboBox = [("tech-model","модель"), ("tech-year","год"),("tech-generation","поколение")]
  , customFunc = return ()
  , switchWidgets  = ("tech-model-input", ["tech-year-input","tech-generation-input"])
  , enableWidgets  = ["tech-model", "tech-model-input"]
  , disableWidgets = ["tech-model-input","tech-year-input","tech-generation-input","form-submit"]
  , events = [onChange, onSelect, onFocus]
  , callFunc = (\f -> do
       val <- f "tech-mark"
       call (AModel val) $ createOptionsByIdWithRefresh "tech-model")
  , valFunc  = parseJVal
  , hideFunc = hideTechGenerationMessage
  }

techModel :: Entity
techModel = Entity
  { selectId = "tech-model"
  , inputId  = "tech-model-input"
  , flushComboBox = [("tech-year","год"),("tech-generation","поколение")]
  , customFunc = return ()
  , switchWidgets  = ("tech-year-input", ["tech-generation-input"])
  , enableWidgets  = ["tech-year", "tech-year-input"]
  , disableWidgets = ["tech-year-input","tech-generation-input","tech-form-submit"]
  , events = [onChange, onSelect, onFocus]
  , callFunc = (\f -> do
        val <- f "tech-model"
        call (AAge val) $ do createOptionsByIdWithRefresh "tech-year")
  , valFunc  = getIntVal
  , hideFunc = hideTechGenerationMessage
  }

techYear :: Entity
techYear = Entity
  { selectId = "tech-year"
  , inputId  = "tech-year-input"
  , flushComboBox = [("tech-generation","поколение")]
  , customFunc = return ()
  , switchWidgets  = ("tech-year-input", [])
  , enableWidgets  = ["tech-generation", "tech-generation-input"]
  , disableWidgets = ["tech-generation-input","tech-form-submit"]
  , events = [onChange, onSelect, onFocus]
  , callFunc = (\f -> do
        mval <- f "tech-model"
        val <- f "tech-year"
        call (AGen (mval,val)) $ processTechMultipleGens "tech-generation")
  , valFunc  = getIntVal
  , hideFunc = restoreAgeWidget
  }

techGeneration :: Entity
techGeneration = Entity
  { selectId = "tech-generation"
  , inputId  = "tech-generation-input"
  , flushComboBox = []
  , customFunc = return ()
  , switchWidgets  = ("tech-generation", ["tech-year"])
  , enableWidgets  = []
  , disableWidgets = ["tech-form-submit"]
  , events = [onChange, onSelect, onFocus]
  , callFunc = (\f -> do
                   x <- f "tech-generation"
                   case x of
                     Nothing -> return ()
                     _ -> restoreAgeWidget)
  , valFunc  = getIntVal
  , hideFunc = return ()
  }

----------------------------------------------------------------------

switchRegionAdvice :: Fay ()
switchRegionAdvice = do
  inputVal <- getJValById "region"
  advice <- getElementById "advice"
  ival <- parseMaybeInt inputVal
  let b1 = or $ map (==ival) eastRegions
  case b1 of
    True  -> do
      setInner advice wrongRegionAdvice
      toggleCurrElem "advice" "show"
      removeClassById "form-submit" "show"
      toggleCurrElem "form-submit" "hide"
      mapM_ disable $
        ["mark-input","model-input","year-input","generation-input","budget","form-submit"]
        ++ ["tech-model-input", "tech-year-input", "tech-generation-input", "tech-form-submit"]
    False -> do
      clean "advice"
      removeClassById "advice" "show"
      toggleCurrElem "form-submit" "show"
      removeClassById "form-submit" "hide"
  
addCommonEvent :: Entity -> Fay ()
addCommonEvent a = do
  btn <- getElementById $ selectId a
  comboValToNull (inputId a) (selectId a)
  propagateAction btn (commonEvent a) (events a)

commonEvent :: Entity -> Element -> Fay ()
commonEvent a btn = do
  hideFunc a
  mapM_ clearComboBoxValueById $ flushComboBox a
  customFunc a
  let f = valFunc a
      g = callFunc a
      sa = selectId a
      sw = switchWidgets a
  refreshComboBox sa
  inputVal <- getJValById sa
  compared <- compareInputSelect sa inputVal
  case compared of
    True  -> do
      widgetSwitch (fst sw) (snd sw)
      mapM_ enableById $ enableWidgets a
      g f
    False -> do
      mapM_ disable $ disableWidgets a


addBudgetEvent :: Fay ()
addBudgetEvent = do
  btn <- getElementById "budget"
  setValue btn ""
  propagateAction btn budgetEvent [onChange, onClick, onFocus, onKeyUp]


budgetProgress :: Element -> Fay ()
budgetProgress btn = do
  val <- getValue btn
  case (T.length val) of
    0 -> do
      disableButton
    _  -> do
      enableButton
      t <- getElementById "form-submit"
      submitEvent t

enableButton :: Fay ()
enableButton = do
  enableById "form-submit"
  removeClassById "form-submit" "btn-disabled"
  toggleCurrElem  "form-submit" "btn-enabled"
  
disableButton :: Fay ()
disableButton = do
  disable "form-submit"
  removeClassById "form-submit" "btn-enabled"
  toggleCurrElem  "form-submit" "btn-disabled"


budgetEvent btn = do
  hideGenerationMessage
  propagateAction btn budgetProgress [onKeyUp]

addSubmitEvent :: Fay ()
addSubmitEvent = do
  btn <- getElementById "form-submit"
  propagateAction btn submitEvent [onClick, onSubmit]

addTechSubmitEvent :: Fay ()
addTechSubmitEvent = do
  btn <- getElementById "tech-form-submit"
  propagateAction btn techSubmitEvent [onClick, onSubmit]

techSubmitEvent :: Element -> Fay ()
techSubmitEvent _ = do
  ma <- getJValById "tech-mark"
  mo <- getComboVal "tech-model"
  ge <- getComboVal "tech-generation"
  ye <- getComboVal "tech-year"
  let r = IR2 ma mo ye ge
  setValueById "tech-q" r


submitEvent :: Element -> Fay ()
submitEvent _ = do
  ma <- getJValById "mark"
  mo <- getComboVal "model"
  re <- getJValById "region"
  ge <- getComboVal "generation"
  ye <- getComboVal "year"
  bu <- getJValById "budget"
  let r = IR re ma mo ye ge bu
  setValueById "q" r

processTechMultipleGens :: T.Text -> [(T.Text, Int)] -> Fay ()
processTechMultipleGens _ [] = return ()
processTechMultipleGens par t = do
  -- get message for generation to be friendly for user
  showById "tech-generation-help"
  v <- techGenerationMessage
  elem <- getElementById "tech-generation-help-msg"
  setInner elem v

  -- hiding budget widget and showing generation widget
  enableById "tech-generation"
  showById "tech-generation-container"
  hideById "tech-year-container"
  createOptionsByIdWithRefresh "tech-generation" t
  gen <- getElementById "tech-generation"
  propagateAction gen (commonEvent techGeneration) [onFocus, onKeyUp]

processMultipleGens :: T.Text -> [(T.Text, Int)] -> Fay ()
processMultipleGens _ [] = return ()
processMultipleGens par t = do
  -- get message for generation to be friendly for user
  showById "generation-help"
  v <- generationMessage
  elem <- getElementById "generation-help-msg"
  setInner elem v

  -- hiding budget widget and showing generation widget
  enableById "generation"
  showById "generation-container"
  hideById "budget-container"
  createOptionsByIdWithRefresh "generation" t
  gen <- getElementById "generation"
  propagateAction gen (commonEvent generation) [onFocus, onKeyUp]


techGenerationMessage :: Fay T.Text
techGenerationMessage = do
  [maVal, moVal, ageVal] <- mapM getOptionText [ "tech-mark", "tech-model", "tech-year" ]
  return $ T.concat [ "Упс! В ", ageVal, " году выпускались одновременно несколько поколений "
                    , maVal, " ", moVal, "! Выберите нужное!"
                    ]
generationMessage :: Fay T.Text
generationMessage = do
  [maVal, moVal, ageVal] <- mapM getOptionText [ "mark", "model", "year" ]
  return $ T.concat [ "Упс! В ", ageVal, " году выпускались одновременно несколько поколений "
                    , maVal, " ", moVal, "! Выберите нужное!"
                    ]
restoreAgeWidget :: Fay ()
restoreAgeWidget = do
  hideTechGenerationMessage
  enableById "tech-form-submit"
  hideById "tech-generation-container"
  showById "tech-year-container"


restoreBudgetWidget :: Fay ()
restoreBudgetWidget = do
  hideGenerationMessage
  enableById "budget"
  hideById "generation-container"
  showById "budget-container"

hideGenerationMessage :: Fay ()
hideGenerationMessage = hideById "generation-help"

hideTechGenerationMessage :: Fay ()
hideTechGenerationMessage = hideById "tech-generation-help"


hideById :: T.Text -> Fay ()
hideById a = do
  removeClassById a "show"
  toggleCurrElem  a "hide"

showById :: T.Text -> Fay ()
showById a = do
  removeClassById a "hide"
  toggleCurrElem a "show"

propagateAction :: Element -> (Element -> Fay ()) -> [(Element -> Fay () -> Fay ())] -> Fay ()
propagateAction btn action events = mapM_ (\x -> x btn $ action btn) events

createOptions :: Element -> [(T.Text,Int)] -> Fay ()
createOptions elem = mapM_ $ createOption elem

createOption :: Element -> (T.Text,Int) -> Fay ()
createOption par (text,val) = do
  elem <- createElement "option"
  appendChild par elem
  setInner elem text
  setValue elem text
  setAttr elem "id" (T.pack $ show val)

createOptionsById :: T.Text -> [(T.Text, Int)] -> Fay ()
createOptionsById eid x = do
  elem <- getElementById eid
  createOptions elem x

createOptionsByIdWithRefresh :: T.Text -> [(T.Text, Int)] -> Fay ()
createOptionsByIdWithRefresh eid x = do
  createOptionsById eid x
  refreshComboBox eid

wrongRegionAdvice :: T.Text
wrongRegionAdvice = "Наши данные по популярности моделей на ДВ и в Сибири могут быть нерелевантными.\nМы работаем над качеством нашего сервиса и будем рады оповестить вас, когда ситуация изменится к лучшему.\nЕсли вам интересно, то можете оставить email в поле ниже."

eastRegions :: [Maybe Int]
eastRegions = [Just 31, Just 42, Just 45, Just 58, Just 74, Just 83, Just 24, Just 25, Just 27, Just 57, Just 32, Just 52, Just 59, Just 67, Just 72, Just 81, Just 82, Just 84, Just 29, Just 23, Just 15, Just 18, Just 20, Just 85, Just 2, Just 4]

addCustomFocusEvent :: (T.Text, T.Text, [T.Text]) -> Fay ()
addCustomFocusEvent (mid, eid, dlist) = do
  btn <- getElementById mid
  onFocus btn $ do
    widgetSwitch eid dlist  

widgetSwitch :: T.Text -> [T.Text] -> Fay ()
widgetSwitch eid dlist = do
  mapM_ disable dlist
  mapM_ clean dlist
  enableById eid
    
clean :: T.Text -> Fay ()
clean eid = do
  elem <- getElementById eid
  setValue elem ""
  clear elem

disable :: T.Text -> Fay ()
disable eid = do
  elem <- getElementById eid
  setAttr elem "disabled" "disabled"

clear :: Element -> Fay ()
clear elem = setInner elem ""

clearOption :: T.Text -> Element -> Fay ()
clearOption t elem = setInner elem $ T.concat [ "<option selected=selected id=0 value=\"\">Введите ", t ]


data Request = IR Region Mark Model Year Generation Budget | IR2 Mark Model Year Generation  deriving Eq

type Region = T.Text
type Mark = T.Text
type Model = T.Text
type Year = T.Text
type Generation = T.Text
type Budget = T.Text

setValueById :: T.Text -> a -> Fay ()
setValueById = ffi "$('#'.concat(%1)).val(JSON.stringify(%2))"

compareInputSelect :: T.Text -> T.Text -> Fay Bool
compareInputSelect eid inputVal = do
  case inputVal of
    ""  -> return False
    "0" -> return False
    _   -> do
      elems <- getJChildren eid
      elemVals <- mapM getValue elems
      return $ or $ map (==inputVal) $ elemVals

replaceEmpty :: T.Text -> Fay T.Text
replaceEmpty val = case T.length val == 0 of
  True  -> return "0"
  False -> return val

clearComboBoxValueById :: (T.Text, T.Text) -> Fay ()
clearComboBoxValueById (eid, opt) = do
  elem <- getElementById eid
  clearOption opt elem
  clean $ T.concat [eid, "-input"]

addSendRequestEvent :: Fay ()
addSendRequestEvent = do
  btn <- getElementById "tech-form-submit"
  propagateAction btn sendRequestEvent [onClick, onSubmit]

sendRequestEvent :: Element -> Fay ()
sendRequestEvent _ = do
  ge <- getJValById "generation"
  em <- getJValById "email"
  let r = IR3 ge em
  setValueById "tech-q" r
  call (ReqAdd r) $ hideRequestForm
  
hideRequestForm :: Bool -> Fay ()
hideRequestForm _ = do
  hideById "rec9267769"
  showById "continue"

trigger :: T.Text -> T.Text -> T.Text -> Fay ()
trigger = ffi "$('#'.concat(%2)).trigger(%1).val(%3)"

triggerEntity :: T.Text -> T.Text -> Fay ()
triggerEntity eid target = do
  elem <- getElementById "autocomplete"
  attrN <- getAttr elem eid
  attrV <- getAttr elem $ T.concat [eid, "-id"]
  log' attrN
  log' attrV
  trigger "change" target attrV
  trigger "change" target attrN
  log' "Done"

triggerEntityN :: Int -> T.Text -> T.Text -> Fay ()
triggerEntityN 0 x y = return ()
triggerEntityN n x y = do
  triggerEntity x y
  triggerEntityN (n-1) x y 
