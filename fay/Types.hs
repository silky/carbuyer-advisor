{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import           FFIExample

import           DOM
import           Data.Text (fromString)
import qualified Data.Text as T
import           Fay.Yesod
import           Prelude hiding (sort)
--import           Data.Ord

main :: Fay ()
main = do
  (elem:_) <- getElementsByName "f1"
  mapM_ hideElem ["f2","f3", "f4", "f5" ]
  switch1 elem
  onChange elem $ (switch1 elem)
  (elem2:_) <- getElementsByName "f2"
  switch2 elem2
  onChange elem2 $ (switch2 elem2)
  
  del <- getElementById "del"
  val <- getValById "type"
         
  let url = T.concat ["/" , val ]
  onClick del $ 
        ajaxDelete url

switch1 :: Element -> Fay ()
switch1 elem = do
    hideElem "f2"     
    showElem "f2"
    val <- getValue elem
    i   <- parseInt val
    let totvs = getTolTypes i tol
        fs = getTotvValues totvs
        ufs = filter (\x -> (notElem x fs)) getAllValues

    mapM_ (hideFormElem "f2") getAllValues
    mapM_ (showFormElem "f2") fs

    (el:_) <- getElementsByName "f2"
    setValue el (head fs)
    switch2 el

switch2 :: Element -> Fay ()
switch2 elem = do
    val <- getValue elem
    i <- parseInt val
    mapM_ hideElem ["f3","f4","f5"]
    lvs <- getTolvs i tol
    showByTolv lvs
    
showByTolv :: TOLV -> Fay ()
showByTolv tlv
  | tlv == tolv1 = mapM_ showElem [ "f3", "f4" ]
  | tlv == tolv2 = showElem "f5"

toggleFormElemByName :: T.Text -> T.Text -> T.Text -> Fay ()
toggleFormElemByName formName elemName tgl = do
   (el:_) <- getElementsByName formName
   nodes <- childNodes el
   elem <- return $ nodeListToArray nodes
   el2  <- mapM getValue elem
   let e' = snd $ head $ filter ((== elemName) . fst) $ zip el2 elem
   rmClass e' "show"
   rmClass e' "hide"
   addClass e' tgl

hideFormElem :: T.Text -> T.Text -> Fay ()
hideFormElem a b = toggleFormElemByName a b "hide"

showFormElem :: T.Text -> T.Text -> Fay ()
showFormElem a b = toggleFormElemByName a b "show"

---------------------------------------------

data TO = TO { toName :: T.Text, toKey :: Int, toIndex :: T.Text } deriving Eq
data TOL = TOL { tolCategory :: TO, tolTypes :: [TOTV] } 
             deriving Eq
data TOTV = TOTV { tolType :: TO, tolValue :: TOLV } deriving Eq
data TOLV = TOLV1 { tolvTop :: T.Text, tolvBottom :: T.Text } | TOLV2 { tolvConst :: T.Text } deriving Eq

tolv1 :: TOLV
tolv1 = TOLV1 "f3" "f4"
tolv2 :: TOLV
tolv2 = TOLV2 "f5"

tolYear :: TO
tolYear = TO "Возраст" (1 :: Int) "f1"

tolPop :: TO
tolPop = TO "Популярность" (2 :: Int) "f1"

tolPrice :: TO
tolPrice = TO "Статистика" (3 :: Int) "f1"

tolRegion :: TO
tolRegion = TO "Регион" (4 :: Int) "f1"

tolYearInterval :: TO
tolYearInterval = TO "Диапазон лет" (1 :: Int) "f2"

tolAdvCountInterval :: TO
tolAdvCountInterval = TO "Диапазон чисел объявлений" (2 :: Int) "f2"

tolPlusPercent :: TO
tolPlusPercent = TO "+%" (3 :: Int) "f2"

tolMinusPercent :: TO
tolMinusPercent = TO "-%" (4 :: Int) "f2"

tolLeft :: TO
tolLeft = TO "Крайнее левое" (5 :: Int) "f2"

tolRight :: TO
tolRight = TO "Крайнее правое" (6 :: Int) "f2"

tolRegionSize :: TO
tolRegionSize = TO "Региональное значение" (7 :: Int) "f2"

totvYearInterval :: TOTV
totvYearInterval = TOTV tolYearInterval tolv1

totvLeft :: TOTV
totvLeft = TOTV tolLeft tolv2

totvRight :: TOTV
totvRight = TOTV tolRight tolv2

totvAdvCountInterval :: TOTV
totvAdvCountInterval = TOTV tolAdvCountInterval tolv1

totvPlusPercent :: TOTV
totvPlusPercent = TOTV tolPlusPercent tolv2

totvMinusPercent :: TOTV
totvMinusPercent = TOTV tolMinusPercent tolv2

totvRegion :: TOTV
totvRegion = TOTV tolRegionSize tolv2

tol :: [TOL]
tol = [ TOL x y | x <- [ tolYear ]
                , y <- [ [ totvYearInterval ] ] ]
      ++ [ TOL x y | x <- [ tolPop ]
                   , y <- [ [ totvAdvCountInterval ] ] ]
      ++ [ TOL x y | x <- [ tolPrice ]
                   , y <- [ [ totvLeft, totvRight, totvPlusPercent, totvMinusPercent ] ] ]
      ++ [ TOL x y | x <- [ tolRegion ]
                   , y <- [ [ totvRegion ] ] ]


getTol :: Int -> [TOL] -> TOL
getTol a b = b !! a

toggleTolTypes :: (Int -> Int -> Bool) -> Int -> [TOL] -> [TOTV]
toggleTolTypes f typeId tolList = nub $ concat $ map tolTypes $ filter ((f typeId) . toKey . tolCategory) tolList

getTolTypes :: Int -> [TOL] -> [TOTV]
getTolTypes = toggleTolTypes (==)

getTotvValues :: [TOTV] -> [T.Text]
getTotvValues = nub . map (T.pack . show . totvKey)

totvKey :: TOTV -> Int
totvKey = toKey . tolType

getAllValues :: [T.Text]
getAllValues = getTotvValues $ nub $ concat $ map tolTypes tol

notGetTolTypes :: Int -> [TOL] -> [TOTV]
notGetTolTypes = toggleTolTypes (/=)

getTolvs :: Int -> [TOL] -> Fay TOLV
getTolvs t tollist = do 
    let totvList1 = nub $ concat $ map tolTypes tollist
        totvKeyList = map totvKey totvList1 
        z = zip totvKeyList totvList1
    sz <- sort z
    return $ tolValue . snd . (!! (t-1)) $ sz
        

toToWidget :: TO -> (T.Text, Int)
toToWidget (TO a b _) = (a, b)

