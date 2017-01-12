module TextOrderer where

import MicrosoftApiCall
import Data.ByteString as B
import Data.List.Split
import Data.List
import Data.Text as T
import Data.Text.Encoding as E
import GHC.Exts

createListOfTuples :: [PicText] -> [((Int, Int), String)]
createListOfTuples xs = sortWith (snd . fst) (Data.List.foldl (\acc x -> acc ++ [(getXYFromBox (boundingBoxText x), text x)]) [] xs)

getXYFromBox :: String -> (Int, Int)
getXYFromBox boxText = (read (numberArray !! 0) :: Int, read (numberArray !! 1) :: Int)
    where
        numberArray = (Data.List.Split.splitOn "," boxText)

getYList :: [((Int, Int), String)] -> [[(Int, String)]]
getYList (x : xs) = getYList' (x : xs) [] [] (getSndValue x)

getYList' :: [((Int, Int), String)] -> [(Int, String)] -> [[(Int, String)]] -> Int -> [[(Int, String)]]
getYList' [] lineAcc acc _ = acc ++ [lineAcc]
getYList' (x : xs) lineAcc acc lastValue
    | abs ((currentY) - lastValue) <= 20 = getYList' xs (lineAcc ++ [xAndString x]) acc currentY
    | otherwise                          = getYList' (x: xs) [] (acc ++ [lineAcc]) currentY
    where
        currentY = getSndValue x

getFinalList :: [[(Int, String)]] -> [[Text]]
getFinalList xs = getFinalList' xs []

getFinalList' :: [[(Int, String)]] -> [[Text]] -> [[Text]]
getFinalList' [] acc = acc
getFinalList' (x: xs) acc = getFinalList' xs (acc ++ [getJustString sortedList])
    where
        sortedList = Data.List.sort x

getJustString :: [(Int, String)] -> [Text]
getJustString xs = Data.List.foldl (\acc x -> acc ++ [T.pack (snd x)]) [] xs

getSndValue :: ((Int, Int), String) -> Int
getSndValue x = (snd . fst) x

xAndString :: ((Int, Int), String) -> (Int, String)
xAndString x = ((fst . fst) x,  snd x)

listOfLines :: [PicText] -> [[Text]]
listOfLines xs = do
    let listOfTuples = createListOfTuples xs
    let listOfX      = getYList listOfTuples
    getFinalList listOfX