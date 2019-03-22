{-# LANGUAGE OverloadedStrings #-}
import Network.AWS.Data
import qualified Data.ByteString.Lazy as LBS
import Network.AWS.Data.ByteString as ByteString
import Data.Maybe
import AWSLambda
import AWSLambda.Events.APIGateway
import Control.Lens
import Data.Aeson as Aeson
import Data.Aeson.Embedded
import Data.HashMap.Strict as HashMap
import Data.Text 
import AuthorizerIAMPolicy
import qualified Amazonka.IAM.Policy as Policy

authorizerMain 
  :: (ToText authResponse)
  => (Aeson.Value -> IO (APIGatewayProxyResponse authResponse))
  -> IO ()
authorizerMain = lambdaMain



main = authorizerMain handler

getString:: Aeson.Value -> Text
getString (String string) = string

getHashMap:: Aeson.Value -> HashMap Text Aeson.Value
getHashMap (Object hashMap) = hashMap

getPropFromObj :: Aeson.Value -> Text -> Aeson.Value 
getPropFromObj obj key = fromMaybe (String "") $  HashMap.lookup key (getHashMap obj)


handler :: Aeson.Value -> IO (APIGatewayProxyResponse ByteString)
handler request = do
  let requestContext= getPropFromObj request "requestContext"
  let accountId = getString $ getPropFromObj requestContext "accountId"
  let methodArn = getString $ getPropFromObj request "methodArn"
  let policy =   fromMaybe HashMap.empty $ decode (encode (enableAccess accountId methodArn)) :: HashMap Text Value
  let authorizer = insert "principalId" "boss" policy
  pure $  responseOK & responseBody ?~ (LBS.toString $ encode (Object authorizer))

