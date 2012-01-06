{-# LANGUAGE OverloadedStrings #-}

module Gwist.Twitter (
    getTwitterCredential,
    postTweet
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Failure
import Data.Default
import Network.HTTP.Conduit
import Web.Authenticate.OAuth (OAuth (..), Credential (..))
import qualified Web.Authenticate.OAuth as OA
import Data.Aeson ((.=), (.:))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Aeson as AE
import qualified Data.Text as T
import Codec.Binary.UTF8.String as UTF8
import System.IO
import Gwist.Config
import qualified Gwist.JSON as JSON

-- to compile yourself, comment out the following line, and
-- replace Secret.twitterConsumer{Key,Secret} with your values
import qualified Gwist.Secret as Secret

endpointURI = "https://api.twitter.com"

oauthParams :: OAuth
oauthParams = OAuth {
                oauthServerName = "twitter",
                oauthRequestUri = "http://twitter.com/oauth/request_token",
                oauthAccessTokenUri = "http://twitter.com/oauth/access_token",
                oauthAuthorizeUri = "http://twitter.com/oauth/authorize",
                oauthConsumerKey = BS8.pack Secret.twitterConsumerKey,
                oauthConsumerSecret = BS8.pack Secret.twitterConsumerSecret,
                oauthSignatureMethod = OA.HMACSHA1,
                oauthCallback = Nothing
              }

getPIN :: String -> IO String
getPIN url = do
  putStrLn $ "visit " ++ url
  putStr "enter PIN: "
  hFlush stdout
  getLine

getTwitterCredential :: IO Credential
getTwitterCredential = do
  cred <- OA.getTemporaryCredential oauthParams
  pin <- getPIN $ OA.authorizeUrl oauthParams cred
  OA.getAccessToken oauthParams $ OA.insert "oauth_verifier" (BS8.pack pin) cred

postTweet :: Credential -> String -> IO ()
postTweet cred message = do
  req <- OA.signOAuth oauthParams cred =<<
           urlEncodedBody [ ("status", BS8.pack $ UTF8.encodeString message),
                            ("wrap_links", "true") ] <$>
           parseUrl (endpointURI ++ "/1/statuses/update.json")
  Response sc _ b <- withManager $ httpLbsRedirect req
  if 200 <= sc && sc < 300 then do
    return ()
  else
    failure $ StatusCodeException sc b
