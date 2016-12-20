module Lib
    ( startApp
    ) where

import MicrosoftApiCall
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
import qualified OpenCV as CV
import qualified Text.Blaze.Html

startApp :: IO ()
startApp = putStrLn "hello"

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.projectoxford.ai" 443 ""

--imageWork :: IO()
imageWork = do
    file <- B.readFile "/home/chuck/Documents/Working/OCR/ocrexperiments/python_image_processing/test/pics/IMG_0650.JPG"
    img <- return $ CV.imdecode CV.ImreadGrayscale file
    manager <- newManager tlsManagerSettings
    result <- postImage (Just "4734fe1f149d45278562726fd47b0393") img (Just "de") (Just "true") manager baseUrl
    putStrLn (show "hallo")