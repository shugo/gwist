{-# LANGUAGE OverloadedStrings #-}

module Gwist.Github (
    Gist (..),
    createGist
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Failure
import Network.HTTP.Enumerator
import Network.HTTP.Types
import qualified Text.JSON as JSON
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as AE
import qualified Data.Text as T
import qualified Gwist.JSON as WJ

data Gist = Gist {
  gistDescription :: String, 
  gistPublic :: Bool,
  gistFilename :: String,
  gistContent :: String,
  gistURL :: String
} deriving Show

instance AE.ToJSON Gist where
  toJSON (Gist description public filename content url) =
    AE.object ["description" .= description,
               "public" .= public,
               "files" .= AE.object [fname .= file]]
      where fname = T.pack filename
            file = AE.object ["content" .= content]

instance AE.FromJSON Gist where
  parseJSON (AE.Object v) =
    Gist <$>
      v .: "description" <*>
      v .: "public" <*>
      pure "" <*>
      pure "" <*>
      v .: "html_url"

  parseJSON _ = mzero

endpointURI = "https://api.github.com"

createGist :: String -> String -> String -> IO (Gist)
createGist desc filename content = do
  req0 <- parseUrl $ endpointURI ++ "/gists"
  let body = AE.encode $ Gist desc True filename content ""
  let req = req0 {
    method = methodPost,
    requestHeaders = [("Content-Type", "application/json; charset=\"utf-8\"")],
    requestBody = RequestBodyLBS body
  }
  Response sc _ b <- liftIO $ withManager $ httpLbsRedirect req
  if 200 <= sc && sc < 300 then
    WJ.readJSON b
  else
    failure $ StatusCodeException sc b
