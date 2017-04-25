{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleAdvice where

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
    let url = T.concat [ "/simple-advice" , "/" , val ]
    onClick input $ 
        ajaxDelete url


