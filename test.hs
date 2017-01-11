{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

[Line
    {boundingBox = "501,991,81,161", lines =
        [PicWord {boundingBoxWord = "501,991,81,49", words =
            [PicText {boundingBoxText = "501,991,81,49", text = "Stk"}]},
         PicWord {boundingBoxWord = "508,1108,12,44", words =
            [PicText {boundingBoxText = "508,1108,12,44", text = "1"}]}]
    },
 Line
    {boundingBox = "528,587,1131,975", lines =
        [PicWord {boundingBoxWord = "586,587,953,57", words =
            [PicText {boundingBoxText = "586,587,201,51", text = "Vinzenz"},
             PicText {boundingBoxText = "822,590,136,51", text = "lottl"},
             PicText {boundingBoxText = "996,594,543,50", text = "Backerei-Konditorei"}]},
         PicWord {boundingBoxWord = "528,643,543,75", words =
            [PicText {boundingBoxText = "528,643,78,48", text = "Adi"},
             PicText {boundingBoxText = "645,646,426,72", text = "Maislinqer-Str."}]},
         PicWord {boundingBoxWord = "1118,648,484,59", words =
            [PicText {boundingBoxText = "1118,655,70,52", text = "12;"},
             PicText {boundingBoxText = "1229,655,140,48", text = "81373"},
             PicText {boundingBoxText = "1402,648,200,54", text = "Munchen"}]},
         PicWord {boundingBoxWord = "1546,1111,113,51", words =
            [PicText {boundingBoxText = "1546,1111,113,51", text = "0,70"}]},
         PicWord {boundingBoxWord = "1546,1226,112,51", words =
            [PicText {boundingBoxText = "1546,1226,112,51", text = "3,85"}]},
         PicWord {boundingBoxWord = "1546,1340,112,52", words =
            [PicText {boundingBoxText = "1546,1340,112,52", text = "6,53"}]},
         PicWord {boundingBoxWord = "1546,1511,112,51", words =
            [PicText {boundingBoxText = "1546,1511,112,51", text = "0.43"}]}]
    },
 Line
    {boundingBox = "678,707,690,576", lines =
        [PicWord {boundingBoxWord = "822,707,517,54", words =
            [PicText {boundingBoxText = "822,707,199,53", text = "ftliale"},
             PicText {boundingBoxText = "1055,711,284,50", text = "Fraunhofer"}]},
    },
 Line
    {boundingBox = "911,2589,653,56", lines =
        [PicWord {boundingBoxWord = "911,2589,653,56", words =
            [PicText {boundingBoxText = "911,2589,110,48", text = "Dank"},
             PicText {boundingBoxText = "1059,2591,78,48", text = "fur"},
             PicText {boundingBoxText = "1176,2591,135,50", text = "Ihren"},
             PicText {boundingBoxText = "1344,2593,220,52", text = "Einkauf."}]}]}]
