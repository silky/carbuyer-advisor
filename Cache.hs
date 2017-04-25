module Cache where

--import Control.Concurrent.MVar
import Database.Persist.Sql
import Import

getDomainData :: forall site. (
    YesodPersist site, YesodPersistBackend site ~ SqlBackend)
  => HandlerT site IO DomainData
getDomainData = runDB $ do
  r <- selectList [] [Asc RegionShowOrder]
  ma <- selectList [] [Asc MarkShowOrder]
  mo <- selectList [] [Asc ModelMarkId, Asc ModelName]
  ages <- selectList [] [Asc AgeId]
  lma <- selectList [] [Asc LkModelAgeShowOrder]
  gen <- selectList [] [Asc GenerationModelId, Asc GenerationBottomAge]
  ltag <- selectList [] [Asc LkTagId]
  tas <- selectList [] [Asc TextAdviseId]
  imgs <- selectList [] [Asc ImageId] 
  return $ DomainData ma mo ages lma gen r ltag tas imgs

getCache :: HandlerT App IO DomainData
getCache = do
  App { appData = mv } <- getYesod
  v <- takeMVar mv
  genCache mv v

genCache :: forall site. (
    YesodPersist site, YesodPersistBackend site ~ SqlBackend)
  => MVar (Maybe DomainData)
  -> Maybe DomainData
  -> HandlerT site IO DomainData
genCache mv Nothing = do
      nd <- getDomainData
      putMVar mv (Just nd)
      liftIO $ return nd
genCache mv d@(Just dd) = do
      putMVar mv d
      liftIO $ return dd

resetCache :: HandlerT App IO ()
resetCache = do
  App { appData = mv } <- getYesod
  nd <- getDomainData
  _ <- takeMVar mv
  putMVar mv $ Just nd
