{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.Lazy as L
import Network.AWS.Data
import Data.ByteString.Lazy 
import Network.AWS.Data.ByteString as ByteString
import Data.Maybe
import AWSLambda
import AWSLambda.Events.APIGateway
import Control.Lens
import Data.Aeson as Aeson
import Data.Aeson.Text
import Data.Aeson.Embedded
import Data.HashMap.Strict as HashMap
import Data.Text 
import AuthorizerIAMPolicy
import qualified Amazonka.IAM.Policy as Policy


main = lambdaMain handler

getString:: Aeson.Value -> Text
getString (String string) = string

getHashMap:: Aeson.Value -> HashMap Text Aeson.Value
getHashMap (Object hashMap) = hashMap

aesonObjectLookup :: Aeson.Value -> Text -> Aeson.Value 
aesonObjectLookup obj key = fromMaybe (String "") $  HashMap.lookup key (getHashMap obj)


handler :: Aeson.Value -> IO (Aeson.Value)
handler request = do
  let requestContext= aesonObjectLookup request "requestContext"
  let accountId = getString $ aesonObjectLookup requestContext "accountId"
  let methodArn = getString $ aesonObjectLookup request "methodArn"  
  pure $ Object $ HashMap.fromList [("principalId",String "boss"),("policyDocument",(toJSON $ enableAccess accountId methodArn))]

