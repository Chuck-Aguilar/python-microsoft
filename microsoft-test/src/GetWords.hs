module GetWords where

import Utils
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import qualified System.Directory as D
import Data.List


---------------------The training text should already be formatted---------------------------------
base = "/home/chuck/Documents/Working/OCR/python-microsoft/microsoft-test/output/"
dataBase = "/home/chuck/Documents/Working/OCR/python-microsoft/microsoft-test/data/"
plz = "plz.txt"

wrds :: T.Text -> [ T.Text ]
wrds bs = filter isWord (filter (not . T.null) $ T.split (isSpace) bs)

allwords :: IO (H.HashMap T.Text Int)
allwords = do
    allwords <- fmap (wrds . T.toLower) $ allTrainingWords directoryFiles
    let wordDict = foldl addToWord H.empty allwords
    return wordDict

directoryFiles :: IO [FilePath]
directoryFiles = do
    files <- D.getDirectoryContents base
    return $ justTXT files

justTXT :: [FilePath] -> [FilePath]
justTXT filePath = [x | x <- filePath, isInfixOf "txt" x]

allTrainingWords :: IO [FilePath] -> IO T.Text
allTrainingWords listPath = do
    listFilesPath <- listPath
    file <- makeFile (length listFilesPath) listFilesPath (return (T.pack ""))
    return file

makeFile :: Int -> [FilePath] -> IO T.Text-> IO T.Text
makeFile 0 filePaths files = files
makeFile count filePaths files = do
                file <- Tio.readFile (base ++ filePaths !! (count -1))
                listFiles <- files
                makeFile (count - 1) filePaths (return (T.append listFiles (T.append file (T.pack "\n"))))

--plzCity :: IO (H.HashMap T.Text T.Text)
plzCity :: IO()
plzCity = do
    allPlzCitiesDoc <- Tio.readFile (dataBase ++ plz)
    allPlzCities <- return $ T.splitOn (T.pack "\n") allPlzCitiesDoc
    print $ allPlzCities