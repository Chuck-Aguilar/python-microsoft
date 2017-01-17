module GetWords where

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import Data.List

---------------------The training text should already be formatted---------------------------------

isSpace ch = (ch == ' ' || ch == '\n')

isWord word = T.any (\x -> 'a' <= x && x <= 'z') word

wrds :: T.Text -> [ T.Text ]
wrds bs = filter isWord (filter (not . T.null) $ T.split (isSpace) bs)


allwords = do
    allwords <- fmap (wrds . T.toLower) $ Tio.readFile "0.txt"
    let wordDict = foldl add' H.empty allwords
    return wordDict


add' h w = H.insert w (c+1) h
    where
        c = H.lookupDefault (0 :: Int)  w h