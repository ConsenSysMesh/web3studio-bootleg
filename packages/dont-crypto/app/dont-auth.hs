{-# LANGUAGE OverloadedStrings #-}
import AWSLambda.Events.APIGateway
import Control.Lens
import Data.Aeson
import Data.Aeson.Embedded
import Data.Text as Text
import AuthorizerIAMPolicy
import qualified Data.ByteString as ByteString
import qualified Amazonka.IAM.Policy as Policy
main = apiGatewayMain handler

handler :: APIGatewayProxyRequest (Embedded Value) -> IO (APIGatewayProxyResponse (Embedded (Policy.Policy Policy.Statement)))
handler request = do
  let requestContext = request ^. agprqRequestContext
  let methodARN = constructARN (requestContext^.prcAccountId) (requestContext^.prcApiId) Nothing (requestContext^.prcHttpMethod) (requestContext^.prcResourcePath) 
  print (responseOK & responseBodyEmbedded ?~ (enableAccess (requestContext^.prcAccountId) methodARN))
  pure $ responseOK & responseBodyEmbedded ?~ (enableAccess (requestContext^.prcAccountId) methodARN)

