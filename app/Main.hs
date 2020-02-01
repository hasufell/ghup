module Main where

import           Control.Error.Util
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.UTF8          as UTF8
import           Data.Dates                     ( getCurrentDateTime
                                                , parseDate
                                                , DateTime(..)
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe
import           Data.Semigroup                 ( (<>) )
import qualified Data.Text                     as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.Format.ISO8601
import           GHup
import           GitHub.Auth
import           GitHub.Data.Repos
import           GitHub.Data.URL
import           HPath
import           Options.Applicative
import           System.Console.Pretty
import           System.Exit
import           Text.Layout.Table



data Options = Options
  { optCommand :: Command
  }

data Command
  = Fork ForkOptions
  | Config ConfigOptions
  | Del DelOptions
  | ListForks ListForkOptions

data ForkOptions = ForkOptions
  {
    repo          :: ByteString
  , newBranch     :: Maybe ByteString
  , repoBasePath  :: Maybe ByteString
  }

data ListForkOptions = ListForkOptions
  {
    lSince        :: Maybe ByteString
  }

data ConfigOptions = ConfigOptions {
    oAuth :: ByteString
  , bPath :: Maybe ByteString
  }


data DelOptions = DelOptions {
  del :: ByteString
  }

opts :: Parser Command
opts = subparser
  (  command "fork"       (Fork <$> (info (forkOpts <**> helper) (progDesc "Fork a repository")))
  <> command "config"     (Config <$> (info (configOpts <**> helper) (progDesc "Set ghup config (such as OAuth)")))
  <> command "delete"     (Del <$> (info (delOpts <**> helper) (progDesc "Delete a forked repository")))
  <> command "list-forks" (ListForks <$> (info (lForkOpts <**> helper) (progDesc "List my forks")))
  )

configOpts :: Parser ConfigOptions
configOpts =
  ConfigOptions
    <$> strOption
          (short 'o' <> long "oauth" <> metavar "OAUTH" <> help
            "The OAUTH token"
          )
    <*> optional
          (strOption
            ((short 'p') <> long "base-path" <> metavar "BASE_PATH" <> help
              "The base path to clone into"
            )
          )

forkOpts :: Parser ForkOptions
forkOpts =
  ForkOptions
    <$> strOption
          (short 'r' <> long "repo" <> metavar "REPO" <> help
            "The repository to fork"
          )
    <*> optional
          (strOption
            (short 'b' <> long "branch" <> metavar "BRANCH" <> help
              "The branch to create after cloning the fork"
            )
          )
    <*> optional
          (strOption
            (short 'p' <> long "repo-path" <> metavar "REPO_PATH" <> help
              "The base path where to clone the repository to"
            )
          )

delOpts :: Parser DelOptions
delOpts = DelOptions <$> strOption
  (short 'r' <> long "repo" <> metavar "REPO" <> help "The REPO fork to delete")

lForkOpts :: Parser ListForkOptions
lForkOpts = ListForkOptions <$> optional
  (strOption
    (short 's' <> long "since" <> metavar "SINCE" <> help
      "The repository to fork"
    )
  )


main :: IO ()
main = do
  -- wrapper to run effects with settings
  let run e = do
        settings <- exceptT
              (\_ -> die
                  . color Red
                  $ "Could not get settings, make sure to run 'ghup config' first"
              )
              pure
            $ getSettings
        (flip runReaderT) settings . runExceptT . withExceptT show $ e
  e <- customExecParser (prefs showHelpOnError) (info (opts <**> helper) idm) >>= \case

    -- fork
    Fork (ForkOptions {..}) -> run $ do
      case repoBasePath of
        Just rbp -> case parseAbs rbp of
          Just p  -> prepareRepoForPR' repo (Just p) newBranch
          Nothing -> liftIO $ die (color Red $ "Repo path must be absolute")
        Nothing -> prepareRepoForPR' repo Nothing newBranch

    -- config
    Config (ConfigOptions {..}) -> do
      p <- maybe (pure Nothing) (fmap Just . parseAbs) bPath
      writeSettings (Settings (OAuth oAuth) p) <&> Right

    -- delete
    Del       (DelOptions {..}     ) -> run $ deleteFork' del

    -- list-forks
    ListForks (ListForkOptions {..}) -> run $ do
      mtime <- liftIO $ case lSince of
        Just t' -> do
          dt <- getCurrentDateTime
          let mt =
                either (const Nothing) Just . parseDate dt . UTF8.toString $ t'
          pure $ mt >>= \t ->
            (parseTimeM
              True
              defaultTimeLocale
              "%Y-%-m-%-d"
              (show (year t) <> "-" <> show (month t) <> "-" <> show (day t)) :: Maybe
                UTCTime
            )
        Nothing -> pure Nothing

      forks <- withExceptT show $ getForks mtime
      let formatted =
            gridString [column expand left def def
                       ,column expand left def def]
              $ fmap
                  (\Repo {..} ->
                    [ (T.unpack . getUrl $ repoHtmlUrl)
                    , formatShow (iso8601Format :: Format Day)
                                 (utctDay $ fromJust repoUpdatedAt)
                    ]
                  )
                  forks
      liftIO $ putStrLn $ formatted
      pure ()

  -- print error, if any
  case e of
    Right () -> pure ()
    Left  t  -> die (color Red $ t)
