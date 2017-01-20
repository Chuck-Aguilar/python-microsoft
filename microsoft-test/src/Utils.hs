module Utils where

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.List

addToWord h w = H.insert w (c+1) h
    where
        c = H.lookupDefault (0 :: Int)  w h

addNGram h w = H.insert w (splitInNGrams w) h

addHamming h word realWord = H.insert word (getHammingDistance word realWord) h

getHammingDistance :: T.Text -> T.Text -> Int
getHammingDistance realWord word = sum [1 | (x1, y1) <- T.zip realWord word, x1 /= y1]

splitInNGrams :: T.Text -> [T.Text]
splitInNGrams word
    | T.length word <= 5 = splitAux (addSpace word) 2
    | otherwise        = splitAux (addSpace word) 3

splitAux :: T.Text -> Int -> [T.Text]
splitAux word n
    | n <= T.length word = T.take n word : splitAux (T.drop 1 word) n
    | otherwise          = []

addSpace :: T.Text -> T.Text
addSpace word = T.pack (' ' : T.unpack word ++ " ")