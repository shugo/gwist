{-# LANGUAGE OverloadedStrings #-}

module Gwist.Config (
    Config (..),
    readConfig,
    writeConfig
    ) where

import Data.Maybe
import Data.Default
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as AE
import qualified Data.Text as T
import qualified Gwist.JSON as JSON
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import Web.Authenticate.OAuth (Credential (..))

data Config = Config {
  githubUser :: Maybe String,
  githubPassword :: Maybe String,
  twitterAccessToken :: Maybe String,
  twitterAccessTokenSecret :: Maybe String,
  twitterCredential :: Maybe Credential
} deriving Show

instance Default Config where
  def = Config {
          githubUser = Nothing,
          githubPassword = Nothing,
          twitterAccessToken = Nothing,
          twitterAccessTokenSecret = Nothing,
          twitterCredential = Nothing
        }

instance AE.ToJSON Credential where
  toJSON (Credential { unCredential = ts }) = AE.toJSON ts

instance AE.FromJSON Credential where
  parseJSON (AE.Array a) =
    Credential <$> mapM AE.parseJSON (V.toList a)

  parseJSON _ = mzero

instance AE.ToJSON Config where
  toJSON (Config gu gp tat tats tc) =
    AE.object $ catMaybes [ ("github_user" .=) <$> gu,
                            ("github_password" .=) <$> gp,
                            ("twitter_access_token" .=) <$> tat,
                            ("twitter_access_token_secret" .=) <$> tats,
                            ("twitter_credential" .=) <$> tc ]

instance AE.FromJSON Config where
  parseJSON (AE.Object v) =
    Config <$>
      v .:? "github_user" <*>
      v .:? "github_password" <*>
      v .:? "twitter_access_token" <*>
      v .:? "twitter_access_token_secret" <*>
      v .:? "twitter_credential"

  parseJSON _ = mzero

readConfig :: FilePath -> IO Config
readConfig path = JSON.readJSON =<< LBS.readFile path

writeConfig :: Config -> FilePath -> IO ()
writeConfig conf path = LBS.writeFile path $ AE.encode conf
