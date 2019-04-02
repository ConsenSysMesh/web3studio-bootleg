{-# LANGUAGE OverloadedStrings #-}

module AuthorizerIAMPolicy where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Data.Text
import Data.Maybe
import qualified Amazonka.IAM.Policy as Policy

theAction :: Policy.Block [Policy.Action]
theAction =  Policy.some [Policy.Action (pack "execute-api:invoke")]

theResource :: Text -> Policy.Block [Policy.Resource]
theResource methodArn = Policy.some [Policy.Resource (methodArn)]


enableAccess :: Text -> Text -> Policy.Policy Policy.Statement
enableAccess accountId  methodArn= Policy.document $ pure Policy.allow { Policy.action = theAction,  Policy.resource = theResource $ methodArn}  

