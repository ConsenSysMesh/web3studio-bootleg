{-# LANGUAGE OverloadedStrings #-}

module AuthorizerIAMPolicy where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Data.Text
import Data.Maybe
import qualified Amazonka.IAM.Policy as Policy
constructARN :: Text -> Text -> Maybe Text-> Text -> Text -> Text
constructARN accountId apiId stage method path= Data.Text.concat [(pack "arn:aws:execute-api:*:"), accountId, (pack ":"), apiId, (pack "/"), fromMaybe "" stage, method , path]

theAction :: Policy.Block [Policy.Action]
theAction =  Policy.some [Policy.Action (pack "execute-api:invoke")]

theResource :: Text -> Policy.Block [Policy.Resource]
theResource methodArn = Policy.some [Policy.Resource (methodArn)]


enableAccess :: Text -> Text -> Policy.Policy Policy.Statement
enableAccess accountId  methodArn= Policy.document $ pure Policy.allow { Policy.action = theAction,  Policy.resource = theResource $ methodArn}  

