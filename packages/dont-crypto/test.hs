import Data.Text
data RequestIdentity = RequestIdentity
  { _riCognitoIdentityPoolId         :: !(Maybe Text)
  , _riAccountId                     :: !(Maybe Text)
  , _riCognitoIdentityId             :: !(Maybe Text)
  , _riCaller                        :: !(Maybe Text)
  , _riApiKey                        :: !(Maybe Text)
  , _riSourceIp                      :: !(Maybe Text)
  , _riCognitoAuthenticationType     :: !(Maybe Text)
  , _riCognitoAuthenticationProvider :: !(Maybe Text)
  , _riUserArn                       :: !(Maybe Text)
  , _riUserAgent                     :: !(Maybe Text)
  , _riUser                          :: !(Maybe Text)
  } deriving (Eq, Show)
