module WordCorrector where

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List

isAlpha ch = ('a' <= ch && ch <= 'z')

wrds :: T.Text -> [ T.Text ]
wrds bs = filter (not . T.null) $ T.split (not . isAlpha) bs

--allwords :: H.HashMap T.Text Int
allwords = do
    allwords <- fmap (wrds . T.toLower) $ T.readFile "big.txt"
    let wordDict = foldl add' H.empty allwords
    print $ wordDict



add' h w = H.insert w (c+1) h
    where
        c = H.lookupDefault (0 :: Int)  w h