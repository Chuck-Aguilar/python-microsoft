{-# LANGUAGE DeriveGeneric #-} --JUST FOR TESTING!!!!!!!

module GetUsefulInfo where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

data ReceiptInfo = ReceiptInfo
    { amount :: String
    , address :: String
    , name :: String
    , date :: String
    , plz :: String
    , city :: String
    } deriving (Eq, Show, Generic)

instance ToJSON ReceiptInfo
instance FromJSON ReceiptInfo

{-encode (ReceiptInfo {amount = "5.0", address = "home", name = "Papa", date = "10.10.16", plz = "95843",
    city = "Las Vegas"})-}


--getInfo :: String

