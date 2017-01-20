module WordCorrector where

import Utils

import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T


correctWord :: T.Text -> H.HashMap T.Text Int -> T.Text
correctWord word trainingWords = decidingMethod word (splitInNGrams word) trainingWords

decidingMethod :: T.Text -> [T.Text] -> H.HashMap T.Text Int -> T.Text
decidingMethod word nGramsWord trainingWords = do
    let candidates = getCandidatesHMap (length nGramsWord) nGramsWord (nWords (H.keys trainingWords)) H.empty
    if H.null candidates
        then word
        else do
            let maxValue       = maximum (H.elems candidates)
            let candidatesKeys = getKeysWithMinDistance (H.keys candidates) candidates maxValue []
            if maxValue < 3 && length candidatesKeys > 1
                then positionCharMethod word candidatesKeys trainingWords
                else do
                    if length candidatesKeys == 1
                        then candidatesKeys !! 0
                        else getMostRepeatedWord candidatesKeys trainingWords

nWords :: [T.Text] -> H.HashMap T.Text [T.Text]
nWords wordDict = foldl addNGram H.empty wordDict

getMostRepeatedWord :: [T.Text] -> H.HashMap T.Text Int -> T.Text
getMostRepeatedWord keys trainingWords = getMostRepeatedWord' keys trainingWords (keys !! 0)

getMostRepeatedWord' :: [T.Text] -> H.HashMap T.Text Int -> T.Text -> T.Text
getMostRepeatedWord' [] trainingWords currentMax   = currentMax
getMostRepeatedWord' (k : keys) trainingWords currentMax
    | kValue > currentValue = getMostRepeatedWord' keys trainingWords k
    | otherwise             = getMostRepeatedWord' keys trainingWords currentMax
    where
        kValue       = trainingWords H.! k
        currentValue = trainingWords H.! currentMax

getMinDistance :: [T.Text] -> H.HashMap T.Text Int -> Int -> Int
getMinDistance [] hashMap minDistance   = minDistance
getMinDistance (k : keys) hashMap minDistance
    | kValue < minDistance = getMinDistance keys hashMap kValue
    | otherwise            = getMinDistance keys hashMap minDistance
    where
        kValue       = hashMap H.! k

getKeysWithMinDistance :: [T.Text] -> H.HashMap T.Text Int -> Int -> [T.Text] -> [T.Text]
getKeysWithMinDistance [] hashMap minDist acc = acc
getKeysWithMinDistance (x : xs) hashMap minDist acc
    | hashMap H.! x == minDist = getKeysWithMinDistance xs hashMap minDist (acc ++ [x])
    | otherwise                = getKeysWithMinDistance xs hashMap minDist acc

positionCharMethod :: T.Text -> [T.Text] -> H.HashMap T.Text Int -> T.Text
positionCharMethod realWord candidates trainingWords = do
    let wordList = [x | x <- candidates, T.length realWord == T.length x]
    if length wordList == 0
        then realWord
        else do
            let winnerWord = foldl (\acc word -> addHamming acc word realWord) H.empty wordList
            let minDistance = getMinDistance (H.keys winnerWord) winnerWord (winnerWord H.! ((H.keys winnerWord) !! 0))
            let keysMinValue = getKeysWithMinDistance (H.keys winnerWord) winnerWord minDistance []
            if length keysMinValue == 1
                then keysMinValue !! 0
                else getMostRepeatedWord keysMinValue trainingWords

getCandidatesHMap :: Int -> [T.Text] -> H.HashMap T.Text [T.Text] -> H.HashMap T.Text Int -> H.HashMap T.Text Int
getCandidatesHMap lastLength [] nWordsGrams candidates = candidates
getCandidatesHMap lastLength (gram : wordNGrams) nWordsGrams candidates = getCandidatesHMap lastLength wordNGrams nWordsGrams (getCandidatesHMap' lastLength gram nWordsGrams (H.keys nWordsGrams) candidates)

getCandidatesHMap' :: Int -> T.Text -> H.HashMap T.Text [T.Text] -> [T.Text] -> H.HashMap T.Text Int -> H.HashMap T.Text Int
getCandidatesHMap' lastLength gram nWordsGrams [] candidates = candidates
getCandidatesHMap' lastLength gram nWordsGrams (x : xs) candidates
    | abs (lastLength - length (nWordsGrams H.! x)) < 3 = do
                                if elem gram (nWordsGrams H.! x)
                                    then getCandidatesHMap' lastLength gram nWordsGrams xs (addToWord candidates x)
                                    else getCandidatesHMap' lastLength gram nWordsGrams xs candidates
    | otherwise                                       = getCandidatesHMap' lastLength gram nWordsGrams xs candidates
