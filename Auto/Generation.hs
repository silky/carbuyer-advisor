{-# LANGUAGE OverloadedStrings #-}
module Auto.Generation where

import Import
import Auto.Parser
import Auto.Client
import Text.HTML.TagSoup
import Prelude ((!!))
import Control.Lens

import qualified Data.ByteString.Char8 as B
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as Sess
import qualified Control.Exception as E


instance HttpRequest B.ByteString where
  getUrl a = B.unpack a
  getOptions _ = W.defaults

instance HttpClient B.ByteString B.ByteString

instance HttpResponse B.ByteString where
  getResponse Nothing = liftIO $ do
    return ""
  getResponse (Just x) = liftIO $ do
    writeFile "output.html" x
    return $ encodeUtf8 x

instance TagRep B.ByteString where 
  toTagRep x = case parseTags x of
                 [a] -> toTagRep a
                 _ -> error $ B.unpack $ B.append "When using a TagRep it must be exactly one tag, you gave: " x

genOpts :: ParsingOptions B.ByteString
genOpts = ParsingOptions
  { scopeBegin = ""
  , scopeEnd = ""
  , patternBegin = "<span class=\"icon select__tick\">"
  , patternEnd = "<form class=\"form form_type_search\">"
  , partitionEdge = "<div class=\"menu-item"
  }

instance Parseable B.ByteString AutoGeneration where
  toExtractedObject ctags = G name url
    where url = B.reverse $ B.takeWhile (/='\"')
                $ B.drop 1 $ B.dropWhile (/='\"') $ B.reverse
                $ fromAttrib "data-bem" (ctags !! 0)
          name = fromTagText $ (!! 0) $ filter isTagText ctags

  toExtractedList _ ts = b
    where tags = ts
          e = partitions ((\x -> and [isTagOpenName "div" x, (fromAttrib "class" x) == "menu-item menu-item_theme_islands i-bem"])) tags
          b = fmap toExtractedObject e
          -- filterClass = "menu-item"
  opts = genOpts

data AutoGeneration = G { gName :: B.ByteString, gUrl :: B.ByteString } deriving Show

main :: IO ()
main = do
  t <- readFile "model_input.txt"
  let l = B.lines t
  s <- Sess.withSession $ \sess -> return sess
  mapM_ (exGeneration s) l

urlCheckAction :: IO ()
urlCheckAction = do
  t <- readFile "url_check.txt"
  let l = B.lines t
  s <- Sess.withSession $ \sess -> return sess
  mapM_ (urlCheck s) l

urlCheck :: Sess.Session -> B.ByteString -> IO ()
urlCheck s l = do
  let url = B.unpack l
  r <- E.try $ Sess.get s url
  let dStatCode x = fromIntegral (x ^. W.responseStatus . W.statusCode)
--      failureStatus 
      failureStatus x = ((dStatCode x) / 100) < 4
  case r of 
    Left (E.SomeException _) -> do
      B.appendFile "checkout.txt" l
      B.appendFile "checkout.txt" "\n"
    Right re' -> case failureStatus re' of
      True -> return ()
      False -> do
        B.appendFile "checkout.txt" l
        B.appendFile "checkout.txt" "\n"

exGeneration :: Sess.Session -> B.ByteString -> IO ()
exGeneration s x = do
  res <- httpAction s x (getOptions x) :: IO B.ByteString
  let tags = getTags res
      ag   = toExtractedList genOpts tags
      ag2  = map (addUrl x) ag
  mapM_ (addFile "output_generation.txt" x) ag2

addUrl :: B.ByteString -> AutoGeneration -> AutoGeneration
addUrl u (G n u2) = G n $ B.concat [u, u2]

addFile :: FilePath -> B.ByteString -> AutoGeneration -> IO ()
addFile f url (G n u) = do
  B.appendFile f $ B.concat [url, ";", n, ";", u]
  B.appendFile f "\n"
