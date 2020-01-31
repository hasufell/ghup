{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans    #-}

module GHup
  (
  -- * Types
    ForkResult(..)
  , CloneMethod(..)
  , ProcessError(..)
  , Settings(..)
  -- * Settings
  , getSettings
  , writeSettings
  -- * Github / Git actions
  , prepareRepoForPR'
  , prepareRepoForPR
  , forkRepository
  , cloneRepository
  , setUpstream
  , createBranch
  , deleteFork'
  , deleteFork
  -- * Parsers
  , parseURL
  , ghURLParser
  -- * Utils
  , _info
  , _warn
  , _err
  )
where

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad.Except
import           Data.Attoparsec.ByteString
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.UTF8          as UTF8
import qualified Data.ByteString.Lazy.UTF8     as LUTF8
import           Data.Functor                   ( (<&>) )
import           Data.Proxy
import qualified Data.Text.Encoding            as E
import           Data.Word8
import           GitHub.Auth
import           GitHub.Data.Name
import           GitHub.Data.URL
import           GitHub.Endpoints.Repos
import           GitHub.Request
import           HPath
import           HPath.IO
import           Prelude                 hiding ( readFile
                                                , writeFile
                                                )
import           System.Console.Pretty
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           System.Posix.ByteString        ( getEnv
                                                , RawFilePath
                                                )
import           System.Posix.Files.ByteString
import qualified System.Posix.Process.ByteString
                                               as SPPB
import           System.Posix.Process.ByteString
                                                ( ProcessStatus )
import qualified System.Posix.User             as PU
import           System.Exit
import           Text.Read                      ( readEither )



    ------------------
    --[ Data types ]--
    ------------------


data ForkResult = ForkResult {
    upstream   :: Repo
  , downstream :: Repo
} deriving (Eq, Show)

data CloneMethod = CloneSSH
                 | CloneHTTP
  deriving (Eq, Show)

data ProcessError = NoSuchPid
                  | ProcessFailed Int
                  | ProcessInterrupted
  deriving (Eq, Show)

data UrlParseResult = UrlParseResult {
    owner :: Name Owner
  , repo :: Name Repo
} deriving (Eq, Show)


data Settings = Settings {
    auth     :: Auth
  , basePath :: Maybe (Path Abs)
} deriving (Eq, Read, Show)


deriving instance Read Auth

instance Read (Path Abs) where
  readsPrec p input =
    let str = readsPrec p input :: [(String, String)]
    in  case str of
          [(s, n)] -> case parseAbs (UTF8.fromString s) of
            Just p' -> [(p', n)]
            Nothing -> []
          _ -> []





    ----------------
    --[ Settings ]--
    ----------------



writeSettings :: Settings -> IO ()
writeSettings settings = do
  sf <- getSettingsFile
  let fileperms =
        ownerWriteMode
          `unionFileModes` ownerReadMode
          `unionFileModes` groupWriteMode
          `unionFileModes` groupReadMode
  writeFile sf (Just fileperms) (u8 . show $ settings)
  _info ("Written config to file " <> (UTF8.toString $ toFilePath sf))


getSettingsFile :: IO (Path Abs)
getSettingsFile = do
  let app_dir = [rel|ghup|] :: Path Rel
  getEnv (u8 "XDG_CONFIG_HOME") >>= \case
    Just config -> do
      pc <- parseAbs config
      pure $ pc </> app_dir
    Nothing -> do
      let config_dir = [rel|.config|] :: Path Rel
      home <- getHomeDirectory >>= parseAbs
      pure $ home </> config_dir </> app_dir


getSettings :: IO (Either String Settings)
getSettings = runExceptT (fromEnv <|> fromFile)

 where
  fromEnv :: ExceptT String IO Settings
  fromEnv = do
    (lift $ getEnv (u8 "GITHUB_TOKEN")) >>= \case
      Just t  -> pure $ Settings (OAuth t) Nothing
      Nothing -> throwError "Not found"
  fromFile :: ExceptT String IO Settings
  fromFile = do
    sf  <- lift $ getSettingsFile
    out <-
      ExceptT
        $ ( flip catchIOError (\e -> pure $ Left $ show e)
          $ fmap Right
          $ readFile sf
          )
    liftEither $ readEither (LUTF8.toString out)




    ----------------------------
    --[ Github / Git actions ]--
    ----------------------------


-- | Same as 'prepareRepoForPR', but gets the auth from the config file
-- and parses the owner/repo from the given repo url string.
prepareRepoForPR' :: ByteString       -- ^ string that contains repo url
                  -> Maybe (Path b)   -- ^ base path where the repo should be cloned
                  -> Maybe ByteString -- ^ PR branch name to switch to
                  -> IO (Either String ())
prepareRepoForPR' repoString mRepobase branch = runExceptT $ do
  UrlParseResult {..} <- liftEither $ parseURL repoString
  Settings {..}       <- ExceptT getSettings
  repobase            <- case mRepobase of
    Just r  -> fmap Just $ lift $ toAbs r
    Nothing -> pure basePath
  ExceptT $ prepareRepoForPR auth owner repo repobase branch



-- | Fork the repository to my account, clone it, add original upstream
-- as remote, optionally switch to the given branch.
prepareRepoForPR :: AuthMethod am
                 => am
                 -> Name Owner
                 -> Name Repo
                 -> Maybe (Path b)   -- ^ base path where the repo should be cloned
                 -> Maybe ByteString -- ^ PR branch name to switch to
                 -> IO (Either String ())
prepareRepoForPR am owner repo repobase branch = runExceptT $ do
  repodest <- case repobase of
    Just rb ->
      ((rb </>) <$> (parseRel $ E.encodeUtf8 $ untagName repo)) >>= lift . toAbs
    Nothing -> (parseRel $ E.encodeUtf8 $ untagName repo) >>= lift . toAbs
  ForkResult {..} <- withExceptT show $ ExceptT $ forkRepository am owner repo
  withExceptT show $ ExceptT $ cloneRepository CloneSSH downstream repodest
  withExceptT show $ ExceptT $ setUpstream upstream repodest
  case branch of
    Just b  -> withExceptT show $ ExceptT $ createBranch b repodest
    Nothing -> pure ()
  lift $ _info
    (  "To change to the repo dir, run:\n\tcd "
    <> (UTF8.toString $ toFilePath repodest)
    )


forkRepository :: AuthMethod am
               => am
               -> Name Owner
               -> Name Repo
               -> IO (Either Error ForkResult)
forkRepository am owner repo = runExceptT $ do
  upstream   <- ExceptT $ github' (repositoryR owner repo)
  downstream <- ExceptT $ github am (forkExistingRepoR owner repo Nothing)
  pure $ ForkResult { .. }


cloneRepository :: CloneMethod
                -> Repo
                -> Path b  -- ^ full path where the repo should be cloned to
                -> IO (Either ProcessError ())
cloneRepository CloneSSH (Repo { repoSshUrl = (Just url) }) dest =
  _clone (E.encodeUtf8 $ getUrl url) (toFilePath dest)
cloneRepository CloneHTTP (Repo { repoCloneUrl = (Just url) }) dest =
  _clone (E.encodeUtf8 $ getUrl url) (toFilePath dest)
cloneRepository _ _ _ = fail "No clone url!"



setUpstream :: Repo   -- ^ upstream
            -> Path b -- ^ full path to repo
            -> IO (Either ProcessError ())
setUpstream (Repo { repoCloneUrl = (Just url) }) repodir = _runGit
  [ u8 "-C"
  , toFilePath repodir
  , u8 "remote"
  , u8 "add"
  , u8 "upstream"
  , (E.encodeUtf8 $ getUrl url)
  ]
setUpstream _ _ = fail "No clone url!"


createBranch :: ByteString  -- ^ branch name
             -> Path b      -- ^ full path to repo
             -> IO (Either ProcessError ())
createBranch branch repodir =
  _runGit [u8 "-C", toFilePath repodir, u8 "checkout", u8 "-b", branch]


-- | Same as deleteFork, but gets the auth from the config file
-- and parses the owner/repo from the given repo url string.
deleteFork' :: ByteString -> IO (Either String ())
deleteFork' repoString = runExceptT $ do
  UrlParseResult {..} <- liftEither $ parseURL repoString
  Settings {..}       <- ExceptT getSettings
  ExceptT $ deleteFork auth owner repo


deleteFork :: AuthMethod am
           => am
           -> Name Owner
           -> Name Repo
           -> IO (Either String ())
deleteFork am owner repo = runExceptT $ do
  (withExceptT show $ ExceptT $ github' (repositoryR owner repo)) >>= \case
    (Repo { repoFork = Just True }) -> pure ()
    _                               -> throwError "Not a fork"
  withExceptT show $ ExceptT $ github am (deleteRepoR owner repo)





    ---------------
    --[ Parsers ]--
    ---------------


parseURL :: ByteString -> Either String UrlParseResult
parseURL = parseOnly ghURLParser


ghURLParser :: Parser UrlParseResult
ghURLParser =
  (\n r ->
      let owner = mkName (Proxy :: Proxy Owner) (E.decodeUtf8 n)
          repo  = mkName (Proxy :: Proxy Repo) (E.decodeUtf8 r)
      in  UrlParseResult { .. }
    )
    <$> (  (   str "https://github.com/"
           <|> str "http://github.com/"
           <|> str "git@github.com:"
           <|> empty'
           )
        *> takeWhile1 (\w -> (w /= _slash) && isAlphaNum w)
        <* word8 _slash
        )
    <*> (takeWhile1 isAlphaNum <* ((str ".git" <|> empty') <* endOfInput))
 where
  str    = string . u8
  empty' = str ""




    ---------------
    --[ Helpers ]--
    ---------------


u8 :: String -> ByteString
u8 = UTF8.fromString

_clone :: ByteString -> ByteString -> IO (Either ProcessError ())
_clone url dest = _runGit [u8 "clone", url, dest]

_toGitError :: Maybe ProcessStatus -> Either ProcessError ()
_toGitError ps = case ps of
  Just (SPPB.Exited ExitSuccess    ) -> Right ()
  Just (SPPB.Exited (ExitFailure i)) -> Left $ ProcessFailed i
  Just (SPPB.Terminated _ _        ) -> Left $ ProcessInterrupted
  Just (SPPB.Stopped _             ) -> Left $ ProcessInterrupted
  Nothing                            -> Left $ NoSuchPid

_runGit :: [ByteString] -> IO (Either ProcessError ())
_runGit args = do
  pid <- executeFile ([rel|git|] :: Path Rel) args
  SPPB.getProcessStatus True True pid <&> _toGitError


getHomeDirectory :: IO RawFilePath
getHomeDirectory = do
  e <- getEnv (u8 "HOME")
  case e of
    Just fp -> pure fp
    Nothing -> do
      h <- PU.homeDirectory <$> (PU.getEffectiveUserID >>= PU.getUserEntryForID)
      pure $ u8 h -- this is a guess


_info :: String -> IO ()
_info = _stderr . color Green

_warn :: String -> IO ()
_warn = _stderr . color Yellow

_err :: String -> IO ()
_err = _stderr . color Red

_stderr :: String -> IO ()
_stderr = hPutStrLn stderr
