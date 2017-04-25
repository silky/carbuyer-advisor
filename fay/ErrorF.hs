{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module ErrorF where

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
  mapM_ disable [ "mark-input","model-input","year-input"
                , "generation-input","budget","form-submit"
                ]
  mapM_ addCommonEvent combos
  addBudgetEvent
  addSubmitEvent
  mapM_ clickInputCombo $ map inputId combos
