module Handler.Models where

import Cache
import Import
import Form
import Data.List (nub, (!!))

getModelsR :: MarkId -> Handler Html
getModelsR markId = do
    c <- getCache
    let marks = getMarks c
        models = getModels c
        generations = getGenerations c
        regions = getRegions c
        lktags = getLkTags c
        eGens = concat $ containedIn (fmap (lkTagGeneration . entityVal) lktags) g generations
        eModels = nub $ filter ((==markId) . modelMarkId . entityVal) $ concat $ containedIn (fmap (generationModelId . entityVal) $ eGens) g models

        textAdvices = getTextAdvices c
        images = getImages c
        itModels = fmap (\x -> getModelWithContent x generations lktags images textAdvices) eModels
        eitModels = fmap mFromMaybe $ filter existenceCheck itModels
        dEitModels = divideThree eitModels

        g = entityKey
    (sAdvice':_) <- getRandomSimpleAdvice

    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        setTitle "Рекомендации по приобретению Б/У автомобилей"
        $(widgetFile "models")
        $(fayFile    "HomeF")
