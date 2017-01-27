module TextCorrector where

import Utils

import Data.List
import Data.Text as T
import Text.Regex.PCRE
import WordCorrector

import qualified Data.HashMap.Strict as H


correctFile :: H.HashMap Text Int -> [[Text]] -> [Text]
correctFile trainingWords receipt = do
    let normalizedText = normalizeText receipt
    let correctedText = Data.List.foldl (\acc x -> acc ++ [T.unwords (getFinalLine x trainingWords)]) [] normalizedText
    correctedText

normalizeText :: [[Text]] -> [[Text]]
normalizeText receipt = Prelude.foldl (\acc x -> acc ++ [normalizeLine x]) [] receipt

normalizeLine :: [Text] -> [Text]
normalizeLine xs = Prelude.foldl (\acc x -> acc ++ [T.replace (T.pack ",") (T.pack ".") (T.pack (stripCharactersLine (T.unpack (T.toLower x)) :: String))]) [] xs


stripCharactersLine :: String -> String
stripCharactersLine text = [ x | x <- text, (elem x (['a'..'z'] :: String)) || (elem x (['0'..'9'] :: String)) || (elem x ("-/.," :: String)) || (elem x (['ß'..'ü'] :: String))]


getFinalLine :: [Text] -> H.HashMap Text Int -> [Text]
getFinalLine line trainingWords = getFinalLine' line [] trainingWords

getFinalLine' :: [Text] -> [Text] ->  H.HashMap Text Int -> [Text]
getFinalLine' [] acc trainingWords = acc
getFinalLine' (word : line) acc trainingWords
    | isNumeric (T.unpack word) || not (isWord word)= getFinalLine' line (acc ++ [word]) trainingWords
    | otherwise                 = getFinalLine' line (acc ++ [correct word trainingWords]) trainingWords

correct :: Text -> H.HashMap Text Int -> Text
correct word trainingWords = correctWord word trainingWords
