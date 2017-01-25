{-# LANGUAGE DeriveGeneric #-} --JUST FOR TESTING!!!!!!!

module GetUsefulInfo where

import Utils
import GetWords

import Data.Aeson
import Data.ByteString.Lazy
import GHC.Generics
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Debug.Trace

data ReceiptInfo = ReceiptInfo
    { amount :: T.Text
    , address :: T.Text
    , name :: T.Text
    , date :: T.Text
    , plz :: T.Text
    , city :: T.Text
    } deriving (Eq, Show, Generic)

instance ToJSON ReceiptInfo
instance FromJSON ReceiptInfo

initialText = T.pack ""
date_separation_characters = ['/', '-', '.', '—']
sume_list = [T.pack "summe", T.pack "total", T.pack "gesamt", T.pack "saldo", T.pack "betrag", T.pack "netto"]
addresse_list = [T.pack "str", T.pack "platz"]


getInfo :: [T.Text] -> H.HashMap T.Text T.Text -> ByteString
getInfo document plzDict = getInfo' 0 True document (ReceiptInfo {amount = initialText, address = initialText,
    name = initialText, date = initialText, plz = initialText, city = initialText}) plzDict

getInfo' :: Int -> Bool -> [T.Text] -> ReceiptInfo -> H.HashMap T.Text T.Text -> ByteString
getInfo' index companyNext [] receiptInfo plzDict = trace ("doc length: " ++ show (index)) $ encode receiptInfo
getInfo' index companyNext (line : document) (ReceiptInfo a ad n d p c) plzDict
    | index == 0 || companyNext == True = trace ("doc length: " ++ show (Prelude.length document)) $ do
        if T.length line >= 4 && isWord line  -- This could be better
            then trace (show (ReceiptInfo a ad line d p c)) $ getInfo' (index + 1) False document (ReceiptInfo a ad line d p c) plzDict
            else trace ("b") $ getInfo' (index + 1) companyNext document (ReceiptInfo a ad n d p c) plzDict
    -- Here the desition (str), (total), etc...
    | otherwise                 = do
        let newLine = trace ("doc length: " ++ show (Prelude.length document)) $ T.toLower line
        let newLine = T.replace (T.pack ",") (T.pack ".") newLine
        if T.isInfixOf (sume_list !! 0) newLine || T.isInfixOf (sume_list !! 1) newLine ||
            T.isInfixOf (sume_list !! 2) newLine || T.isInfixOf (sume_list !! 3) newLine ||
            T.isInfixOf (sume_list !! 4) newLine || T.isInfixOf (sume_list !! 5) newLine
            then getInfo' (index + 1) False document (ReceiptInfo (getSumme newLine) ad n d p c) plzDict
            else if T.isInfixOf (addresse_list !! 0) newLine || T.isInfixOf (addresse_list !! 1) newLine
                then getInfo' (index + 1) False document (ReceiptInfo a (fillAddress ad newLine) n d p c) plzDict
                else do
                    let p = fst (getPLZCity newLine plzDict)
                    let c = snd (getPLZCity newLine plzDict)
                    let d = getDate newLine
                    getInfo' (index + 1) False document (ReceiptInfo a ad n d p c) plzDict


-- | otherwise                 = getInfo' (index + 1) False document (ReceiptInfo a ad n d p c)

getSumme :: T.Text -> T.Text
getSumme line = do
    let convert = T.isInfixOf (T.pack "netto") line
    let newLine = T.splitOn (T.pack " ") line
    let check = T.pack ""
    let amount = getSumme' check  newLine
    if convert
        then getBruttoAmount amount
        else amount

getSumme' :: T.Text -> [T.Text] -> T.Text
getSumme' check [] = check
getSumme' check (word:line) = do
    let newWord = T.replace (T.pack "€") (T.pack "") word
    if isNumber check && isNumber newWord
        then T.append check (T.append (T.pack ".") newWord)
        else if isNumber newWord
            then getSumme' newWord line
            else if isNumeric (T.unpack newWord)
                then newWord
                else getSumme' check line


getBruttoAmount :: T.Text -> T.Text
getBruttoAmount amount = T.pack $ show (myRound ((getDoubleNumber (T.unpack amount)) * 1.19) 2)


fillAddress :: T.Text -> T.Text -> T.Text
fillAddress address currentLine
    | address == initialText = currentLine
    | otherwise              = address


getPLZCity :: T.Text -> H.HashMap T.Text T.Text -> (T.Text, T.Text)
getPLZCity line plzDict = do
    let newLine = T.splitOn (T.pack " ") line
    getPLZCity' newLine plzDict

getPLZCity' :: [T.Text] -> H.HashMap T.Text T.Text -> (T.Text, T.Text)
getPLZCity' [] plzDict = (T.pack "", T.pack "")
getPLZCity' (word : lineList) plzDict
    | H.member word plzDict = (word, plzDict H.! word)
    | otherwise             = getPLZCity' lineList plzDict


getDate :: T.Text -> T.Text
getDate line = do
    let newLine = T.splitOn (T.pack " ") line
    getDate' newLine

getDate' :: [T.Text] -> T.Text
getDate' [] = T.pack ""
getDate' (word : line)
    | T.length word == 10 || T.length word == 8 = do
        if isNumber (T.take 2 word) && isNumber (takeManyFrom 2 3 word) && isNumber (T.drop 6 word) &&
            (Prelude.elem (T.index word 2) date_separation_characters && Prelude.elem (T.index word 5) date_separation_characters)
            then word
            else if T.length word == 8
                then if isNumber (T.take 1 word) && isNumber (T.pack (T.index word 2 : "")) && isNumber (T.drop 4 word) &&
                        (Prelude.elem (T.index word 1) date_separation_characters && Prelude.elem (T.index word 3) date_separation_characters)
                        then word
                        else getDate' line
                else getDate' line
    | T.length word == 9 || T.length word == 7 = do
        if isNumber (T.take 2 word) && isNumber (T.pack (T.index word 3 : "")) && isNumber (T.drop 5 word) &&
            (Prelude.elem (T.index word 2) date_separation_characters && Prelude.elem (T.index word 4) date_separation_characters)
            then word
            else getDate' line
    | T.length word == 6                       = do
        if isNumber (T.take 1 word) && isNumber (T.pack (T.index word 2 : "")) && isNumber (T.drop 4 word) &&
            (Prelude.elem (T.index word 1) date_separation_characters && Prelude.elem (T.index word 3) date_separation_characters)
            then word
            else getDate' line
    | otherwise                                = getDate' line
