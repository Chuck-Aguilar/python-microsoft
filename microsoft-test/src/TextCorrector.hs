module TextCorrector where

import Utils

import Data.List
import Data.Text as T
import Text.Regex.PCRE
import WordCorrector

import qualified Data.HashMap.Strict as H

pat = "[a-z]|[0-9]|ä|ö|ü|ß|-|/|.|," :: String

correctFile :: H.HashMap Text Int -> [[Text]] -> [Text]
correctFile trainingWords receipt = do
    let normalizedText = normalizeText receipt
    let correctedText = Data.List.foldl (\acc x -> acc ++ [T.unwords (getFinalLine x trainingWords)]) [] normalizedText
    correctedText

normalizeText :: [[Text]] -> [[Text]]
normalizeText receipt = Prelude.foldl (\acc x -> acc ++ [normalizeLine x]) [] receipt

normalizeLine :: [Text] -> [Text]
normalizeLine xs = Prelude.foldl (\acc x -> acc ++ [T.pack (stripCharactersLine (T.unpack (T.toLower x)) :: String)]) [] xs


stripCharactersLine :: String -> String
stripCharactersLine text = Data.List.intercalate "" (getAllTextMatches (text =~ pat  :: AllTextMatches [] String))


getFinalLine :: [Text] -> H.HashMap Text Int -> [Text]
getFinalLine line trainingWords = getFinalLine' line [] trainingWords

getFinalLine' :: [Text] -> [Text] ->  H.HashMap Text Int -> [Text]
getFinalLine' [] acc trainingWords = acc
getFinalLine' (word : line) acc trainingWords
    | isNumeric (T.unpack word) = getFinalLine' line (acc ++ [word]) trainingWords
    | otherwise                 = getFinalLine' line (acc ++ [correct word trainingWords]) trainingWords

correct :: Text -> H.HashMap Text Int -> Text
correct word trainingWords = correctWord word trainingWords
