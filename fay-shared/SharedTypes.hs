{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SharedTypes where

import Prelude
import Data.Data
import Fay.Yesod
import Data.Text (Text)

data Command = AModel (Maybe Int) (Returns [(Text, Int)])
             | AAge   (Maybe Int) (Returns [(Text, Int)])
             | AGen   (Maybe Int,Maybe Int) (Returns [(Text, Int)])
             | LkAdd  (Maybe Int, Maybe Int) (Returns (Maybe (Text, Text, Text, Text)))
             | LkDel  (Maybe Int, Maybe Int) (Returns Bool)
             | ImgAdd (Maybe Int, [Text]) (Returns [(Text, Text)])
             | ImgDel (Maybe Int) (Returns Bool)
             | ReqAdd (NewRequest) (Returns Bool)
             | PAdd   (PropertyRequest) (Returns (Maybe PropertyRequest))
             | PriceMatrix [Price] (Returns Bool)
    deriving (Typeable, Data)

data NewRequest = IR3 { ir3ge :: Text, ir3em :: Text
                      }
    deriving (Typeable, Data)

data PropertyRequest = PRInsert { priK :: Text, priV :: Text, priD :: Text }
  deriving (Typeable, Data)

data Price =
  Price { val :: Text
        , serv :: Maybe Int
        , hyp :: Maybe Int
        } deriving (Typeable, Data)
data PriceV =
  PriceV { valV :: Double
         , servV :: Int
         , hypV :: Int
         , validated :: Bool
         } 
