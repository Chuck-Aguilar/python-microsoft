module TextOrderer where

import MicrosoftApiCall
import Data.List.Split
import Data.List
import GHC.Exts

createListOfTuples :: [PicText] -> [((Int, Int), String)]
createListOfTuples xs = sortWith (snd . fst) (foldl (\acc x -> acc ++ [(getXYFromBox (boundingBoxText x), text x)]) [] xs)

getXYFromBox :: String -> (Int, Int)
getXYFromBox boxText = (read (numberArray !! 0) :: Int, read (numberArray !! 1) :: Int)
    where
        numberArray = (splitOn "," boxText)

getYList :: [((Int, Int), String)] -> [[(Int, String)]]
getYList (x : xs) = getYList' (x : xs) [] [] (getSndValue x)

getYList' :: [((Int, Int), String)] -> [(Int, String)] -> [[(Int, String)]] -> Int -> [[(Int, String)]]
getYList' [] lineAcc acc _ = acc ++ [lineAcc]
getYList' (x : xs) lineAcc acc lastValue
    | abs ((currentY) - lastValue) <= 20 = getYList' xs (lineAcc ++ [xAndString x]) acc currentY
    | otherwise                          = getYList' (x: xs) [] (acc ++ [lineAcc]) currentY
    where
        currentY = getSndValue x

getFinalList :: [[(Int, String)]] -> [[String]]
getFinalList xs = getFinalList' xs []

getFinalList' :: [[(Int, String)]] -> [[String]] -> [[String]]
getFinalList' [] acc = acc
getFinalList' (x: xs) acc = getFinalList' xs (acc ++ [getJustString sortedList])
    where
        sortedList = sort x

getJustString :: [(Int, String)] -> [String]
getJustString xs = foldl (\acc x -> acc ++ [snd x]) [] xs

getSndValue :: ((Int, Int), String) -> Int
getSndValue x = (snd . fst) x

xAndString :: ((Int, Int), String) -> (Int, String)
xAndString x = ((fst . fst) x,  snd x)

listOfLines :: [PicText] -> [[String]]
listOfLines xs = do
    let listOfTuples = createListOfTuples xs
    let listOfX      = getYList listOfTuples
    getFinalList listOfX