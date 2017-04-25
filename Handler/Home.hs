module Handler.Home where

import Cache
import Import
import Form
import Data.List (nub, scanl')

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    c <- getCache
    let marks = getMarks c
        generations = getGenerations c
        regions = getRegions c
        lktags = getLkTags c
        eGens = concat $ containedIn (fmap (lkTagGeneration . entityVal) lktags) g generations
        eMarks = nub $ concat $ containedIn (fmap (generationMarkId . entityVal) eGens) g marks
        textAdvices = getTextAdvices c
        images = getImages c
        itMarks = fmap (\x -> getMarkWithContent x generations lktags images textAdvices) eMarks
        eitMarks = sortBy (comparing $ (\(x,_,_) -> markName . entityVal $ x)) $ fmap mFromMaybe $ filter existenceCheck itMarks

        eitLen = length eitMarks
        six = 6 :: Int
        eitGroups = eitLen `div` six
        eitDiv = eitLen `mod` six
        f :: Int -> Int -> Int -> [(Int,Int)]
        f x y z = let l = fmap succ (take x (replicate y z)) ++ drop x (replicate y z)
                      m = take y $ scanl' (+) 0 l
                  in zip l m
        ixEits = f eitDiv six eitGroups
        eits = fmap (\x -> take (fst x) $ drop (snd x) eitMarks) ixEits
        g = entityKey

    (sAdvice':_) <- getRandomSimpleAdvice
    tas <- getRandomPromos
    
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        setTitle "Рекомендации по приобретению Б/У автомобилей"
        $(widgetFile "homepage")
        $(fayFile    "HomeF")

postHomeR :: Handler Html
postHomeR = redirect HomeR
