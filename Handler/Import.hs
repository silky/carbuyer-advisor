{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Import where

import Import hiding (length, span)
import Form
import HSH.Command
import System.Directory 
import Data.List (length)
import Codec.Archive.Zip
import Text.Regex.Posix
import Data.Text (span)
import Path.IO (resolveDir', resolveFile')

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Codec.Text.IConv as I

getImportR :: Handler Html
getImportR = do
  ((_,fileWidget),enctype) <- runFormPost fileForm
  (tas' :: [Entity TextAdvise]) <- runDB $ selectList [] [Asc TextAdviseName]
  tags <- runDB $ selectList [LkTagTextAdviseId <-. (fmap entityKey tas')] [Asc LkTagId]
  let tas = fmap (flip hasGeneration tags) tas'
  defaultLayout $ do
    addStylesheet $ StaticR css_bootstrap_css
    setTitle "Import"
    $(widgetFile "import")

postImportR :: Handler Html
postImportR = do
  ((result,_),_) <- runFormPost fileForm
  let msubmission = case result of
        FormSuccess f -> Just f
        _             -> Nothing
  res <- migration msubmission
  runDB $ insertMany_ res
  redirectUltDest ImportR

migration ::
  forall (m :: * -> *).
  MonadIO m =>
  Maybe FileInfo -> m [TextAdvise]
migration (Just f) = liftIO $ do
  let fn = fileName f
      fp = getFilePath dir fn
      dir = "tmp/"
      unpDir = "tmp/extracted/"
  fileMove f fp
  res <- extractZip unpDir fp
  res2 <- mapM (parseAndLoad unpDir) res
  flushDir
  return res2

migration Nothing = return []

getFilePath :: Text -> Text -> FilePath
getFilePath dir fname = unpack $ concat [ dir, fname ]

extractZip :: Text -> String -> IO [String]
extractZip unpdir fp = do
  path <- resolveDir' $ unpack unpdir
  fc   <- resolveFile' $ unpack fp
  withArchive fc (unpackInto path)

  dirCont <- getDirectoryContents $ unpack unpdir
  return $ filter ((> 2) . length) dirCont

parseAndLoad :: Text -> FilePath -> IO TextAdvise
parseAndLoad dir f = do
  fc <- fmap (LB.toStrict . I.convert "cp1251" "utf8") $ LB.readFile $ (unpack dir) ++ f
  let outf = "tmp/converted/" ++ f
  B.writeFile outf fc
  fc2 <- B.readFile outf
  let fcl = B.lines fc2
      h   = Textarea $ conv $ takeWhile (compare' _unwanted) fcl
      uh  = Textarea $ conv $ takeWhile (compare' _lookup)
              $ dropWhile (compare' _unwanted) $ fcl
      l   = Textarea $ conv $ dropWhile (compare' _lookup) fcl
      conv = decodeUtf8 . B.unlines
      compare' a = not . (flip (=~) a) . unpack . take 13 . decodeUtf8
      compare' :: String -> ByteString -> Bool
      ft = pack f
      res = TextAdvise ft ft h uh l Nothing
  return res

flushDir :: IO ()
flushDir = 
  let c = "find tmp -name '*.txt' -type f -exec rm {} \\;"
      c :: String
  in do
    (_ :: String) <- liftIO $ run c
    return ()

_unwanted :: forall a. IsString a => a
_unwanted = "Ч(ег|т)о не брать"
_lookup :: forall a. IsString a => a
_lookup   = "На что обрати"

     
toVars :: Text -> [Text]
toVars a = f [] a
  where f :: [Text] -> Text -> [Text]
        f xs "" = xs
        f xs x =
          let (b,c) = span (/='_') x
          in f (b:xs) c

hasGeneration :: Entity TextAdvise -> [Entity LkTag] -> (Entity TextAdvise, Bool)
hasGeneration ta tags =
  let ftags = equalTo (entityKey ta) (lkTagTextAdviseId . entityVal) tags
  in if null ftags then (ta, False) else (ta, True)
