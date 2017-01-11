module JSONInterpreter where

import MicrosoftApiCall

jsonParser :: [Line] -> [PicText]
jsonParser [] = []
jsonParser xs = listLinesToListPicText xs

listLinesToListPicText :: [Line] -> [PicText]
listLinesToListPicText xs = foldl (\acc x -> acc ++ (listPicWordToListPicText (MicrosoftApiCall.lines x))) [] xs

listPicWordToListPicText :: [PicWord] -> [PicText]
listPicWordToListPicText xs = foldl (\acc x -> acc ++ (MicrosoftApiCall.words x)) [] xs
