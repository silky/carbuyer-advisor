module Handler.Marks where

import Cache
import Import hiding (groupBy, zip)
import Form
import Data.List (nub, (!!), groupBy)
import Prelude (zip)

getMarksR :: Handler Html
getMarksR = do
    c <- getCache
    let marks = getMarks c
        models = getModels c
        generations = getGenerations c
        regions = getRegions c
        lktags = getLkTags c
        eGens = concat $ containedIn (fmap (lkTagGeneration . entityVal) lktags) g generations
        eModels = concat $ containedIn (fmap (generationModelId . entityVal) eGens) g models
        eMarks = nub $ concat $ containedIn (fmap (generationMarkId . entityVal) eGens) g marks
        textAdvices = getTextAdvices c
        images = getImages c
        itMarks = fmap (\x -> getMarkWithContent x generations lktags images textAdvices) eMarks
        eitMarks = fmap mFromMaybe $ filter existenceCheck itMarks
        dEitMarks = divideThree eitMarks
        g = entityKey
    (sAdvice':_) <- getRandomSimpleAdvice

    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        setTitle "Рекомендации по приобретению Б/У автомобилей"
        $(widgetFile "marks")
        $(fayFile    "HomeF")

