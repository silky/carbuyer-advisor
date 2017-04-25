module Handler.Cache where

import Import
import Cache

getCacheR :: Handler Html
getCacheR = do
  resetCache
  redirect HomeR
