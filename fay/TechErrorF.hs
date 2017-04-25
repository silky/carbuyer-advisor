{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module TechErrorF where

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
  let techCombos = [techMark, techModel, techYear, techGeneration]
  mapM_ disable [ "tech-model-input","tech-year-input"
                , "tech-generation-input", "tech-form-submit"
                ]
  mapM_ addCommonEvent techCombos 
  addTechSubmitEvent
  mapM_ clickInputCombo $ map inputId techCombos
