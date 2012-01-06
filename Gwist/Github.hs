{-# LANGUAGE OverloadedStrings #-}

module Gwist.Github (
    Gist (..),
    GistFile (..),
    createGist
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Failure
import Data.Default
import Network.HTTP.Conduit
import Data.Aeson ((.=), (.:))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as AE
import qualified Data.Text as T
import Gwist.Config
import qualified Gwist.JSON as JSON

data GistFile = GistFile {
  gistFilename :: String,
  gistContent :: LBS.ByteString
} deriving Show

data Gist = Gist {
  gistDescription :: String,
  gistPublic :: Bool,
  gistFiles :: [GistFile],
  gistURL :: String
} deriving Show

instance Default Gist where
  def = Gist {
          gistDescription = "",
          gistPublic = True,
          gistFiles = [],
          gistURL = ""
        }

instance AE.ToJSON Gist where
  toJSON (Gist description public files _) =
    AE.object ["description" .= description,
               "public" .= public,
               "files" .= (AE.object $ map fileToPair files)]
      where fileToPair (GistFile name content) =
              T.pack name .= AE.object ["content" .= content]

instance AE.FromJSON Gist where
  parseJSON (AE.Object v) =
    (\u -> def { gistURL = u }) <$> v .: "html_url"

  parseJSON _ = mzero

endpointURI = "https://api.github.com"

setGistBody :: Gist -> Request a -> Request a
setGistBody gist req =
  req {
    method = "POST",
    requestHeaders = [("Content-Type", "application/json; charset=\"utf-8\"")],
    requestBody = RequestBodyLBS $ AE.encode gist
  }

optBasicAuth :: Config -> Request a -> Request a
optBasicAuth Config {githubUser = Just user, githubPassword = Just pass} req =
  applyBasicAuth (BS8.pack user) (BS8.pack pass) req
optBasicAuth _ req = req

createGist :: Config -> Gist -> IO (Gist)
createGist conf gist = do
  req <- (optBasicAuth conf) . (setGistBody gist) <$>
           parseUrl (endpointURI ++ "/gists")
  Response sc _ b <- withManager $ httpLbsRedirect req
  if 200 <= sc && sc < 300 then do
    newGist <- JSON.readJSON b
    return def { gistURL = gistURL newGist }
  else
    failure $ StatusCodeException sc b
