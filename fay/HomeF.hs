{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module HomeF where

import           FFIExample

import           DOM
import           Data.Text (fromString)
import qualified Data.Text as T
import           Fay.Yesod
--import           Fay.Text (toJSON)
import           Prelude hiding (sort)
import           FFI

import SharedTypes
import Widget

main :: Fay ()
main = do
  addComboBox
  let combos = [region, mark, model, age, generation]
      techCombos = [techMark, techModel, techYear, techGeneration]
      allCombos = combos ++ techCombos
  mapM_ disable [ "mark-input","model-input","year-input"
                , "generation-input","budget","form-submit"
                , "tech-model-input","tech-year-input"
                , "tech-generation-input", "tech-form-submit"
                ]
  mapM_ addCommonEvent allCombos
  addBudgetEvent
  addSubmitEvent
  addTechSubmitEvent
  mapM_ clickInputCombo $ map inputId allCombos

  timer <- setTimeout 1000 (\_ -> triggerEntityN 2 "mark" "tech-mark")
  timer2 <- setTimeout 2000 (\_ -> (triggerEntityN 3 "model" "tech-model"))
  timer3 <- setTimeout 3000 (\_ -> (triggerEntityN 3 "year" "tech-year"))
  timer4 <- setTimeout 4000 (\_ -> (triggerEntityN 3 "generation" "tech-generation"))
  return ()
