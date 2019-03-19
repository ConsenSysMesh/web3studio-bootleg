{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
import DontDynamoDB
import AWSLambda.Events.APIGateway
import Control.Lens
import Control.Exception.Lens (handling)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Aeson
import Data.Aeson.Embedded
import Network.AWS.DynamoDB
import Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import Network.AWS.DynamoDB.Types
 

main = apiGatewayMain handler


--function that gets queue url
handler :: APIGatewayProxyRequest (Embedded Value) -> IO (APIGatewayProxyResponse (Embedded (HashMap Text AttributeValue)))
handler request = do
  print $ request ^. requestBody
  let key = attributeValue & avS ?~ "5"
  let attr = attributeValue & avS ?~ (pack "hello")
  insertItem Ohio (pack "Dont") (Map.fromList [((pack "id"), key),((pack "attr1"), attr)])
  item <- DontDynamoDB.getItem Ohio (pack "Dont") "attr1" (Map.fromList [((pack "id"), key)])
  pure $ responseOK & responseBodyEmbedded ?~  ( item ^. girsItem)



