module TextCorrector where

import Data.Text as T
import Data.Text.ICU.Normalize

correctFile :: [[Text]] -> [[Text]]
correctFile receipt = do
    let normalizedText = normalizeText receipt
    normalizedText

normalizeText :: [[Text]] -> [[Text]]
normalizeText receipt = Prelude.foldl (\acc x -> acc ++ [normalizeLine x]) [] receipt

normalizeLine :: [Text] -> [Text]
normalizeLine xs = Prelude.foldl (\acc x -> acc ++ [normalize FCD x]) [] xs





