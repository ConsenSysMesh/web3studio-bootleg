{-# LANGUAGE OverloadedStrings #-}
import AWSLambda.Events.APIGateway
import Control.Lens
import Data.Aeson
import Data.Aeson.Embedded
main = apiGatewayMain handler

handler :: APIGatewayProxyRequest (Embedded Value) -> IO (APIGatewayProxyResponse (Embedded [Int]))
handler request = do
  putStrLn "This should go to logs"
  print $ request ^. requestBody
  pure $ responseOK & responseBodyEmbedded ?~ [1,2,3]



