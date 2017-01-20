module Lib
    ( startApp
    ) where

import GetWords
import MicrosoftApiCall
import JSONInterpreter
import TextCorrector
import TextOrderer
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
import Debug.Trace
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.API.ContentTypes
import Servant.Client
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Aeson.Parser
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as H
import qualified OpenCV as CV
import qualified Text.Blaze.Html


startApp :: IO ()
startApp = imageWork

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.projectoxford.ai" 443 ""

imageWork :: IO ()
imageWork = do
    --file <- B.readFile "/home/chuck/Documents/Working/OCR/ocrexperiments/python_image_processing/test/pics/IMG_0650.JPG"
    file <- B.readFile "/home/chuck/Documents/Working/OCR/ocrexperiments/python_image_processing/test/pics/IMG_0001.JPG"
    --file <- B.readFile "/home/chuck/Documents/Working/OCR/ocrexperiments/python_image_processing/test/pics/IMG_0002.JPG"
    img <- return $ CV.imdecode CV.ImreadGrayscale file
    manager <- newManager tlsManagerSettings
    result <- runExceptT $ postImage (Just "4734fe1f149d45278562726fd47b0393") file (Just "de") (Just "true") manager baseUrl
    case result of
      Left err -> print err
      Right json -> do
        trainingWords <- allwords
        let receipt = listOfLines (jsonParser (regions json))
        print $ correctFile trainingWords receipt
