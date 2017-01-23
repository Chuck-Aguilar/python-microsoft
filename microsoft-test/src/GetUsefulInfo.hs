{-# LANGUAGE DeriveGeneric #-} --JUST FOR TESTING!!!!!!!

module GetUsefulInfo where

import Utils

import Data.Aeson
import Data.ByteString.Lazy
import GHC.Generics
import qualified Data.Text as T

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


getInfo :: [T.Text] -> ByteString
getInfo document = getInfo' 0 True document (ReceiptInfo {amount = initialText, address = initialText,
    name = initialText, date = initialText, plz = initialText, city = initialText})

getInfo' :: Int -> Bool -> [T.Text] -> ReceiptInfo -> ByteString
getInfo' index companyNext [] receiptInfo = encode receiptInfo
getInfo' index companyNext (line : document) (ReceiptInfo a ad n d p c)
    | index == 0 || companyNext = do
        if T.length line >= 4 && isWord line  -- This could be better
            then getInfo' (index + 1) False document (ReceiptInfo a ad line d p c)
            else getInfo' (index + 1) companyNext document (ReceiptInfo a ad n d p c)
    -- Here the desition (str), (total), etc...
    | otherwise                 = getInfo' (index + 1) False document (ReceiptInfo a ad n d p c)


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


--getPLZCity :: T.Text -> (T.Text, T.Text)
--getPLZCity line = do
    --let newLine = T.splitOn (T.pack " ") line
