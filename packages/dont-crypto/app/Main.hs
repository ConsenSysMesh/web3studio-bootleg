{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
import DontDynamoDB
import Network.AWS.Easy
import AWSLambda.Events.APIGateway
import Data.ByteString
import Control.Lens
import Control.Exception.Lens (handling)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text as Text
import Data.Aeson
import Data.Aeson.Embedded
import Network.AWS.DynamoDB
import System.IO
import Network.AWS.Data
import Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import Network.AWS.DynamoDB.Types
import Network.AWS.SQS 
    (
        _QueueDoesNotExist
      , createQueue
      , getQueueURL
      , gqursQueueURL
      , listQueues
      , lqrsQueueURLs
      , mBody
      , receiveMessage
      , rmrsMessages
      , sendMessage
      , SendMessageResponse
      , sqs
    )
    

main = apiGatewayMain handler


newtype QueueURL = QueueURL Text deriving Show

--function that gets queue url
handler :: APIGatewayProxyRequest (Embedded Value) -> IO (APIGatewayProxyResponse (Embedded [Int]))
handler request = do
  print $ request ^. requestBody
  insertItem Ohio False (Encoding.encodeUtf8 (Data.Text.pack "hostname")) 80 (Data.Text.pack "Dont") (Map.fromList [((Data.Text.pack "id"), (attributeValue & avS .~ Just (Text.pack "hello")))])  
  pure $ responseOK & responseBodyEmbedded ?~ [1,2,3]



