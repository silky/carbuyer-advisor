{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module TechRequestF where

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
  addSendRequestEvent
  btn <- getElementById "tech-form-submit"
  onClick btn $ sendRequestEvent btn

  return ()
