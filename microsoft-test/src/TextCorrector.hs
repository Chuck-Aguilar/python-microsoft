module TextCorrector where

import Data.List
import Data.Text as T
import Data.Text.ICU.Normalize
import Text.Regex.PCRE

pat = "[a-z]|[A-Z]|[0-9]|ä|ö|ü|ß|-|/|.|," :: String

correctFile :: [[Text]] -> [[Text]]
correctFile receipt = do
    let normalizedText = normalizeText receipt
    normalizedText

normalizeText :: [[Text]] -> [[Text]]
normalizeText receipt = Prelude.foldl (\acc x -> acc ++ [normalizeLine x]) [] receipt

normalizeLine :: [Text] -> [Text]
normalizeLine xs = Prelude.foldl (\acc x -> acc ++ [T.pack (stripCharactersLine (T.unpack x) :: String)]) [] xs


stripCharactersLine :: String -> String
stripCharactersLine text = Data.List.intercalate "" (getAllTextMatches (text =~ pat  :: AllTextMatches [] String))

-------------This could be optimized, it's not necessary to iterate two times the list -------------

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False

isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s


getFinalLine :: [Text] -> [Text]
getFinalLine line = getFinalLine' line []

getFinalLine' :: [Text] -> [Text] -> [Text]
getFinalLine' (word : line) acc
    | isNumeric (T.unpack word) = getFinalLine' line (acc ++ [word])
    | otherwise                 = []


