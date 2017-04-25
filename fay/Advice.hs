{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Advice where

import           FFIExample

import           DOM
import           Data.Text (fromString)
import qualified Data.Text as T
import           Fay.Yesod
import           Prelude

main :: Fay ()
main = do
    input <- getElementById "del"
    val   <- getValById "advice"
    let url = T.concat [ "/advice" , "/" , val ]
    onClick input $ 
        ajaxDelete url


