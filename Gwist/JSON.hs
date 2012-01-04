{-# LANGUAGE DeriveDataTypeable #-}

module Gwist.JSON (
    InvalidJSON,
    Result (..),
    decodeJSON,
    readJSON
    ) where

import Data.Typeable
import Control.Exception
import Control.Failure
import qualified Data.Attoparsec as AP
import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

data InvalidJSON = InvalidJSON deriving (Typeable, Show)
instance Exception InvalidJSON

data Result a = Success a | Error

decodeJSON :: AE.FromJSON a => LBS.ByteString -> Result a
decodeJSON b =
  case AP.parse (fmap AE.fromJSON AE.json) (lbsToBs b) of
    AP.Done _ (AE.Success g) -> Success g
    _ -> Error
  where lbsToBs = BS.concat . LBS.toChunks

readJSON :: AE.FromJSON a => LBS.ByteString -> IO a
readJSON b =
  case decodeJSON b of
    Success g -> return g
    Error -> failure InvalidJSON
