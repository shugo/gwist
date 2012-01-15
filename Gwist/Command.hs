{-# LANGUAGE DeriveDataTypeable #-}

module Gwist.Command (
    run
    ) where

import Control.Applicative
import Control.Monad.Trans.Resource (ResourceT, runResourceT, ResourceIO)
import qualified Data.Default as D
import qualified Data.ByteString.Lazy as LBS
import System.IO
import System.Environment
import System.Directory as SD
import System.Console.CmdArgs
import System.FilePath.Posix as FP
import Codec.Binary.UTF8.String as UTF8
import Gwist.Config
import Gwist.Github
import Gwist.Twitter

data Params = Params {
    config :: Maybe String,
    user :: Maybe String,
    message :: String,
    files :: [FilePath]
} deriving (Show, Data, Typeable)

gwistVersion = "0.0"

gwistParams = Params {
  config = def &= typFile &= help "Specify config file",
  user = def &= typ "USER" &= help "Specify Github username",
  message = def &= argPos 0 &= typ "MESSAGE",
  files = def &= args &= typ "FILES"
} &= program "gwist"
  &= summary ("gwist version " ++ gwistVersion)

configPath :: Params -> IO FilePath
configPath (Params { config = Just file }) = return file
configPath (Params { config = Nothing }) =
  (++ "/.gwistrc") <$> SD.getHomeDirectory

askPassword :: IO String
askPassword = do
  putStr "Github password: "
  hFlush stdout
  oldEcho <- hGetEcho stdin
  hSetEcho stdin False
  line <- getLine
  hSetEcho stdin oldEcho
  putChar '\n'
  return line

checkGithubUser :: Params -> Config -> IO Config
checkGithubUser params conf =
  case user params of
    Just u -> do
      pass <- askPassword
      return conf { githubUser = Just u, githubPassword = Just pass }
    Nothing -> return conf

checkTwitterCredential :: Config -> IO Config
checkTwitterCredential conf =
  case twitterCredential conf of
    Just cred -> return conf
    Nothing -> do
      cred <- getTwitterCredential
      return conf { twitterCredential = Just cred }

getConfig :: Params -> IO Config
getConfig params = (readConfig =<< configPath params) `catch`
                     (\e -> return D.def)

loadConfig :: Params -> IO Config
loadConfig params =
  checkGithubUser params =<< checkTwitterCredential =<< getConfig params

saveConfig :: Params -> Config -> IO ()
saveConfig params conf = writeConfig conf =<< configPath params

createGistFile :: FilePath -> IO GistFile
createGistFile filename =
  GistFile (FP.takeFileName $ UTF8.decodeString filename) <$>
           LBS.readFile filename

run :: IO ()
run = do
  params <- cmdArgs gwistParams
  conf <- loadConfig params
  let desc = UTF8.decodeString $ message params
  gistFiles <- mapM createGistFile $ files params
  msg <- if null gistFiles then
           return desc
         else do
           let g = D.def { gistDescription = desc, gistFiles = gistFiles }
           newG <- runResourceT $ createGist conf g
           putStrLn $ "Created a gist at " ++ gistURL newG
           return $ desc ++ " " ++ gistURL newG
  case twitterCredential conf of
    Just cred -> do
      postTweet cred $ msg
      putStrLn "Posted a tweet"
    Nothing -> error "Twitter credential not found"
  saveConfig params conf
