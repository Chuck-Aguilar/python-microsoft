import qualified Data.Text as T
import System.Directory as D

simpleFunc :: Bool -> [Int] -> Bool
simpleFunc companyNext [] = companyNext
simpleFunc companyNext (x:xs)
    | x<= 5 || companyNext = do
        if x == 0
            then simpleFunc False xs
            else simpleFunc companyNext xs
    | otherwise            = simpleFunc companyNext xs