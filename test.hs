import qualified Data.Text as T
import System.Directory as D

getHammingDistance :: T.Text -> T.Text -> Int
getHammingDistance realWord word = sum [1 | (x1, y1) <- T.zip realWord word, x1 /= y1]

texts = D.listDirectory "output/"