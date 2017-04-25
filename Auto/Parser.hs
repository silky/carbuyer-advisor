{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}
module Auto.Parser where

import Text.HTML.TagSoup hiding (renderTags)
import Text.StringLike
import Prelude 

getTags :: (TagRep a, StringLike a) => a -> [Tag a]
getTags a = parseTags a

extractTags :: (TagRep a, StringLike a) => a -> a -> [Tag a] -> [Tag a]
extractTags begin end tags = takeWhile (~/= end) . dropWhile (~/= begin) $ tags

class (TagRep a, StringLike a) => Parseable a b where
  toExtractedList :: ParsingOptions a ->  [Tag a] -> [b]
  toExtractedList _ [] = []
  toExtractedList po ts = b
    where e = partitions (~== (partitionEdge po)) ts
          b = fmap toExtractedObject e

  toExtractedObject :: [Tag a] -> b
  opts :: ParsingOptions a

data ParsingOptions a = ParsingOptions
  { scopeBegin :: a
  , scopeEnd   :: a
  , partitionEdge :: a
  , patternBegin :: a
  , patternEnd :: a
  } 
