{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
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
  , getForks
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
import           Control.Monad.Except    hiding ( fail )
import           Control.Monad.Fail
import           Control.Monad.Reader    hiding ( fail )
import           Data.Attoparsec.ByteString
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as B
import qualified Data.ByteString.UTF8          as UTF8
import qualified Data.ByteString.Lazy.UTF8     as LUTF8
import           Data.Functor                   ( (<&>) )
import           Data.List
import           Data.Proxy
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import           Data.Time.Clock
import           Data.Word8
import           GHC.Exts                       ( toList )
import           GitHub.Auth
import           GitHub.Data.Name
import           GitHub.Data.URL
import           GitHub.Data.Request
import           GitHub.Endpoints.Repos
import           GitHub.Endpoints.Search
import           GitHub.Endpoints.Users
import           GitHub.Request
import           HPath
import           HPath.IO
import           Prelude                 hiding ( readFile
                                                , writeFile
                                                , fail
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
    _auth     :: Auth
  , _basePath :: Maybe (Path Abs)
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



writeSettings :: (MonadThrow m, MonadIO m) => Settings -> m ()
writeSettings settings = do
  sf <- getSettingsFile
  let fileperms =
        ownerWriteMode
          `unionFileModes` ownerReadMode
          `unionFileModes` groupWriteMode
          `unionFileModes` groupReadMode
  liftIO $ writeFile sf (Just fileperms) (u8 . show $ settings)
  _info ("Written config to file " <> (UTF8.toString $ toFilePath sf))


getSettingsFile :: (MonadThrow m, MonadIO m) => m (Path Abs)
getSettingsFile = do
  let app_dir = [rel|ghup|] :: Path Rel
  (liftIO $ getEnv (u8 "XDG_CONFIG_HOME")) >>= \case
    Just config -> do
      pc <- parseAbs config
      pure $ pc </> app_dir
    Nothing -> do
      let config_dir = [rel|.config|] :: Path Rel
      home <- liftIO (getHomeDirectory >>= parseAbs)
      pure $ home </> config_dir </> app_dir


getSettings :: (MonadThrow m, MonadIO m) => ExceptT String m Settings
getSettings = (fromEnv <|> fromFile)

 where
  fromEnv :: MonadIO m => ExceptT String m Settings
  fromEnv = do
    (liftIO $ getEnv (u8 "GITHUB_TOKEN")) >>= \case
      Just t  -> pure $ Settings (OAuth t) Nothing
      Nothing -> throwError "Not found"
  fromFile :: (MonadThrow m, MonadIO m) => ExceptT String m Settings
  fromFile = do
    sf  <- getSettingsFile
    out <-
      ExceptT
      $ liftIO
      $ (flip catchIOError (\e -> pure $ Left $ show e) $ fmap Right $ readFile
          sf
        )
    liftEither $ readEither (LUTF8.toString out)




    -------------------------------------
    --[ Combined Github / Git actions ]--
    -------------------------------------


-- | Same as 'prepareRepoForPR', but gets the auth from the config file
-- and parses the owner/repo from the given repo url string.
prepareRepoForPR' :: ( MonadIO m
                     , MonadReader Settings m
                     , MonadFail m
                     , MonadThrow m
                     )
                  => ByteString       -- ^ string that contains repo url
                  -> Maybe (Path b)   -- ^ base path where the repo should be cloned
                  -> Maybe ByteString -- ^ PR branch name to switch to
                  -> ExceptT Error m ()
prepareRepoForPR' repoString mRepobase branch = do
  UrlParseResult {..} <- (liftEither $ parseURL repoString) ?* uError
  repobase            <- case mRepobase of
    Just r  -> fmap Just $ liftIO $ toAbs r
    Nothing -> basePath
  prepareRepoForPR owner repo repobase branch



-- | Fork the repository to my account, clone it, add original upstream
-- as remote, optionally switch to the given branch.
prepareRepoForPR :: ( MonadIO m
                    , MonadReader Settings m
                    , MonadFail m
                    , MonadThrow m
                    )
                 => Name Owner
                 -> Name Repo
                 -> Maybe (Path b)   -- ^ base path where the repo should be cloned
                 -> Maybe ByteString -- ^ PR branch name to switch to
                 -> ExceptT Error m ()
prepareRepoForPR owner repo repobase branch = do
  repodest <- case repobase of
    Just rb ->
      ((rb </>) <$> (parseRel $ E.encodeUtf8 $ untagName repo))
        >>= liftIO
        .   toAbs
    Nothing -> (parseRel $ E.encodeUtf8 $ untagName repo) >>= liftIO . toAbs
  ForkResult {..} <- (forkRepository owner repo) ?* (uError . show)
  (ExceptT $ cloneRepository CloneSSH downstream repodest) ?* (uError . show)
  (ExceptT $ setUpstream upstream repodest) ?* (uError . show)
  case branch of
    Just b  -> (ExceptT $ createBranch b repodest) ?* (uError . show)
    Nothing -> pure ()
  lift $ _info
    (  "To change to the repo dir, run:\n\tcd "
    <> (UTF8.toString $ toFilePath repodest)
    )



    -------------------
    --[ Git actions ]--
    -------------------


cloneRepository :: (MonadIO m, MonadFail m)
                => CloneMethod
                -> Repo
                -> Path b  -- ^ full path where the repo should be cloned to
                -> m (Either ProcessError ())
cloneRepository CloneSSH (Repo { repoSshUrl = (Just url) }) dest =
  _clone (E.encodeUtf8 $ getUrl url) (toFilePath dest)
cloneRepository CloneHTTP (Repo { repoCloneUrl = (Just url) }) dest =
  _clone (E.encodeUtf8 $ getUrl url) (toFilePath dest)
cloneRepository _ _ _ = fail "No clone url!"



setUpstream :: (MonadIO m, MonadFail m)
            => Repo   -- ^ upstream
            -> Path b -- ^ full path to repo
            -> m (Either ProcessError ())
setUpstream (Repo { repoCloneUrl = (Just url) }) repodir = _runGit
  [ u8 "-C"
  , toFilePath repodir
  , u8 "remote"
  , u8 "add"
  , u8 "upstream"
  , (E.encodeUtf8 $ getUrl url)
  ]
setUpstream _ _ = fail "No clone url!"


createBranch :: MonadIO m
             => ByteString  -- ^ branch name
             -> Path b      -- ^ full path to repo
             -> m (Either ProcessError ())
createBranch branch repodir =
  _runGit [u8 "-C", toFilePath repodir, u8 "checkout", u8 "-b", branch]





    ----------------------
    --[ Github actions ]--
    ----------------------


forkRepository :: (MonadIO m, MonadReader Settings m)
               => Name Owner
               -> Name Repo
               -> ExceptT Error m ForkResult
forkRepository owner repo = do
  upstream   <- github_ (repositoryR owner repo)
  downstream <- githubAuth (forkExistingRepoR owner repo Nothing)
  pure $ ForkResult { .. }


-- | Same as deleteFork, but gets the auth from the config file
-- and parses the owner/repo from the given repo url string.
deleteFork' :: (MonadIO m, MonadReader Settings m)
            => ByteString
            -> ExceptT Error m ()
deleteFork' repoString = do
  UrlParseResult {..} <- (liftEither $ parseURL repoString) ?* uError
  deleteFork owner repo


deleteFork :: (MonadIO m, MonadReader Settings m)
           => Name Owner
           -> Name Repo
           -> ExceptT Error m ()
deleteFork owner repo = do
  github_ (repositoryR owner repo) >>= \case
    (Repo { repoFork = Just True }) -> pure ()
    _ -> throwError (uError "Not a fork")
  githubAuth (deleteRepoR owner repo)


getForks :: (MonadIO m, MonadReader Settings m)
         => Maybe UTCTime
         -> ExceptT Error m [Repo]
getForks mtime = do
  user <- githubAuth userInfoCurrentR
  let userName = untagName $ userLogin user
  repos <- github_
    (searchReposR $ mconcat [T.pack "user:", userName, T.pack " fork:only"])
  pure $ sortBy (\x y -> compare (repoUpdatedAt y) (repoUpdatedAt x)) $ filter
    (\case
      Repo { repoFork = Just True, repoUpdatedAt = Just t } ->
        maybe True (t >=) mtime
      _ -> False
    )
    (toList $ searchResultResults repos)




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
        *> takeWhile1 (/= _slash)
        <* word8 _slash
        )
    <*> parseRepoName
 where
  str    = string . u8
  empty' = str ""
  parseRepoName :: Parser ByteString
  parseRepoName = do
    c <- fmap B.singleton anyWord8
    r <- many1' ((str ".git" <* endOfInput) <|> fmap B.singleton anyWord8)
    if last r == u8 ".git"
      then pure $ mconcat (c : (init r))
      else pure (mconcat (c : r)) <* endOfInput



    ---------------
    --[ Helpers ]--
    ---------------


u8 :: String -> ByteString
u8 = UTF8.fromString

_clone :: MonadIO m => ByteString -> ByteString -> m (Either ProcessError ())
_clone url dest = _runGit [u8 "clone", url, dest]

_toGitError :: Maybe ProcessStatus -> Either ProcessError ()
_toGitError ps = case ps of
  Just (SPPB.Exited ExitSuccess    ) -> Right ()
  Just (SPPB.Exited (ExitFailure i)) -> Left $ ProcessFailed i
  Just (SPPB.Terminated _ _        ) -> Left $ ProcessInterrupted
  Just (SPPB.Stopped _             ) -> Left $ ProcessInterrupted
  Nothing                            -> Left $ NoSuchPid

_runGit :: MonadIO m => [ByteString] -> m (Either ProcessError ())
_runGit args = liftIO $ do
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


_info :: MonadIO m => String -> m ()
_info = liftIO . _stderr . color Green

_warn :: MonadIO m => String -> m ()
_warn = liftIO . _stderr . color Yellow

_err :: MonadIO m => String -> m ()
_err = liftIO . _stderr . color Red

_stderr :: MonadIO m => String -> m ()
_stderr = liftIO . hPutStrLn stderr


auth :: MonadReader Settings m => m Auth
auth = asks _auth

basePath :: MonadReader Settings m => m (Maybe (Path Abs))
basePath = asks _basePath


githubAuth :: ( MonadReader Settings m
              , MonadIO m
              , ParseResponse mt req
              , res ~ Either Error req
              )
           => (GenRequest mt rw req)
           -> ExceptT Error m req
githubAuth req = do
  a <- auth
  ExceptT $ liftIO $ github a req


github_ :: (MonadIO m, ParseResponse mt req, res ~ Either Error req, ro ~ 'RO)
        => (GenRequest mt ro req)
        -> ExceptT Error m req
github_ req = do
  ExceptT $ liftIO $ github' req


-- | Flipped 'withExceptT'.
(?*) :: Functor m => ExceptT e m a -> (e -> e') -> ExceptT e' m a
(?*) = flip withExceptT


uError :: String -> Error
uError = UserError . T.pack
