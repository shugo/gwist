{-# LANGUAGE DeriveDataTypeable #-}

module Gwist.JSON (
    Result (..),
    decodeJSON,
    readJSON
    ) where

import Data.Typeable
import qualified Data.Attoparsec as AP
import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

data Result a = Success a | Error String

decodeJSON :: AE.FromJSON a => LBS.ByteString -> Result a
decodeJSON b =
  case AP.parse (fmap AE.fromJSON AE.json) (lbsToBs b) of
    AP.Done _ (AE.Success g) -> Success g
    AP.Done _ (AE.Error msg) -> Error msg
    AP.Fail _ _ msg -> Error msg
    AP.Partial _ -> Error "JSON is too short"
  where lbsToBs = BS.concat . LBS.toChunks

readJSON :: AE.FromJSON a => LBS.ByteString -> IO a
readJSON b =
  case decodeJSON b of
    Success g -> return g
    Error msg -> error msg
