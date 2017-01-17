module WordCorrector where

import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T


correctWord :: T.Text -> H.HashMap T.Text Int -> T.Text
correctWord word trainingWords = decidingMethod word (splitInNGrams word) trainingWords

decidingMethod :: T.Text -> [T.Text] -> H.HashMap T.Text Int -> T.Text
decidingMethod word nGramsWord trainingWords = word

-----------------------------------------Utils------------------------------------------------

nWords :: [T.Text] -> H.HashMap T.Text [T.Text]
nWords wordDict = foldl add' H.empty wordDict

add' h w = H.insert w (splitInNGrams w) h

splitInNGrams :: T.Text -> [T.Text]
splitInNGrams word
    | T.length word <= 5 = splitAux (addSpace word) 2
    | otherwise        = splitAux (addSpace word) 3

addSpace :: T.Text -> T.Text
addSpace word = T.pack (' ' : T.unpack word ++ " ")

splitAux :: T.Text -> Int -> [T.Text]
splitAux word n
    | n <= T.length word = T.take n word : splitAux (T.drop 1 word) n
    | otherwise          = []

getMostRepeatedWord :: [T.Text] -> H.HashMap T.Text Int -> T.Text
getMostRepeatedWord keys trainingWords = getMostRepeatedWord' keys trainingWords (keys !! 0)

getMostRepeatedWord' :: [T.Text] -> H.HashMap T.Text Int -> T.Text -> T.Text
getMostRepeatedWord' [] trainingWords currentMax   = currentMax
getMostRepeatedWord' (k : keys) trainingWords currentMax
    | kValue > currentValue = getMostRepeatedWord' keys trainingWords k
    | otherwise             = getMostRepeatedWord' keys trainingWords currentMax
    where
        kValue       = getValueFromHash k trainingWords
        currentValue = getValueFromHash currentMax trainingWords


getMinDistance :: [T.Text] -> H.HashMap T.Text Int -> Int -> Int
getMinDistance [] hashMap minDistance   = minDistance
getMinDistance (k : keys) hashMap minDistance
    | kValue < minDistance = getMinDistance keys hashMap kValue
    | otherwise            = getMinDistance keys hashMap minDistance
    where
        kValue       = getValueFromHash k hashMap


getValueFromHash :: T.Text -> H.HashMap T.Text Int -> Int
getValueFromHash word hashMap = fromJust (H.lookup word hashMap)

getHammingDistance :: T.Text -> T.Text -> Int
getHammingDistance realWord word = sum [1 | (x1, y1) <- T.zip realWord word, x1 /= y1]

----Refact all add_functions it could be just one-----
addHamming h word realWord = H.insert word (getHammingDistance word realWord) h

positionCharMethod :: T.Text -> [T.Text] -> H.HashMap T.Text Int -> T.Text
positionCharMethod realWord candidates trainingWords = do
    let wordList = [x | x <- candidates, T.length realWord == T.length x]
    if length wordList == 0
        then realWord
        else do
            let winnerWord = foldl (\acc word -> addHamming acc word realWord) H.empty wordList
            let minDistance = getMinDistance (H.keys winnerWord) winnerWord (getValueFromHash ((H.keys winnerWord) !! 0) winnerWord)
            (H.keys winnerWord) !! 0
            ----Make a list with the minValues and return the most repeated of those keys (line 92 in python)