module MicrosoftApiCall
    (
        postImage
    )where


import Data.Aeson
import Data.Aeson.Compat
import Data.Typeable
import Data.Aeson.Types
import Foreign.C.Types
import GHC.Generics
import Network.HTTP.Client (Manager)
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.API
import Servant.API.ContentTypes
import Servant.Client
import qualified Data.ByteString as B
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M

data TextPic = TextPic
    { language :: String
    , textAngle :: Double
    , orientation :: String
    , regions :: [Line]
    } deriving (Eq, Show, Generic)

instance ToJSON TextPic
instance FromJSON TextPic

data Line = Line
    { boundingBox :: String
    , lines :: [PicWord]
    } deriving (Eq, Show, Generic)

instance ToJSON Line
instance FromJSON Line

data PicWord = PicWord
    { boundingBoxWord :: String
    , words :: [PicText]
    } deriving (Eq, Show, Generic)

instance ToJSON PicWord
instance FromJSON PicWord

data PicText = PicText
    { boundingBoxText :: String
    , text :: String
    } deriving (Eq, Show, Generic)

instance ToJSON PicText
instance FromJSON PicText

type TextPicAPI = "/vision/v1.0/ocr" :> Header "Ocp-Apim-Subscription-Key" String
                                     :> ReqBody '[OctetStream] B.ByteString
                                     :> QueryParam "language" String
                                     :> QueryParam "detectOrientation" String
                                     :> Post '[JSON] TextPic

microsoftAPI :: Proxy TextPicAPI
microsoftAPI = Proxy

postImage :: Maybe String -> B.ByteString -> Maybe String -> Maybe String -> Manager -> BaseUrl -> ClientM TextPic
postImage = client microsoftAPI



