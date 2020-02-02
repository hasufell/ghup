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
import qualified Data.Text.Encoding            as E
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.Format.ISO8601
import           Data.Traversable
import           GHup
import           GitHub.Auth
import           GitHub.Data.Gists
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
  | CreateGist CreateGistOptions
  | ListGist ListGistOptions

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

data CreateGistOptions = CreateGistOptions {
    input :: Input
  , description :: Maybe ByteString
  , private :: Bool
}

data Input
  = FileInput [ByteString]
  | StdInput

data ListGistOptions = ListGistOptions
  {
    lgSince        :: Maybe ByteString
  , lgDesc         :: Bool
  }

fileInput :: Parser Input
fileInput =
  FileInput
    <$> (some
          (strOption
            (long "file" <> short 'f' <> metavar "FILENAME" <> help
              "Input one or more files"
            )
          )
        )

stdInput :: Parser Input
stdInput = flag' StdInput (long "stdin" <> help "Read from stdin")

inputP :: Parser Input
inputP = fileInput <|> stdInput


opts :: Parser Command
opts = subparser
  (  command
      "fork"
      (Fork <$> (info (forkOpts <**> helper) (progDesc "Fork a repository")))
  <> command
       "config"
       (   Config
       <$> (info (configOpts <**> helper)
                 (progDesc "Set ghup config (such as OAuth)")
           )
       )
  <> command
       "delete"
       (   Del
       <$> (info (delOpts <**> helper)
                 (progDesc "Delete a forked repository")
           )
       )
  <> command
       "list-forks"
       (   ListForks
       <$> (info (lForkOpts <**> helper) (progDesc "List my forks"))
       )
  <> command
       "gistc"
       (CreateGist <$> (info (cGistOpts <**> helper) (progDesc "Create gist"))
       )
  <> command
       "gistl"
       (ListGist <$> (info (lGistOpts <**> helper) (progDesc "List gists")))
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

cGistOpts :: Parser CreateGistOptions
cGistOpts =
  CreateGistOptions
    <$> inputP
    <*> optional
          (strOption
            (short 'd' <> long "description" <> metavar "DESCRIPTION" <> help
              "The description of the gist (optional)"
            )
          )
    <*> switch
          (short 'p' <> long "private" <> help
            "Whether gist should be private (default: public)"
          )

lGistOpts :: Parser ListGistOptions
lGistOpts =
  ListGistOptions
    <$> optional
          (strOption
            (short 's' <> long "since" <> metavar "SINCE" <> help
              "The repository to fork"
            )
          )
    <*> switch
          (short 'd' <> long "descriptions" <> help
            "Whether to show descriptions (default: False)"
          )


main :: IO ()
main = do
  -- wrapper to run effects with settings
  let
    run e = do
      settings <-
        exceptT
            (\_ ->
              die
                . color Red
                $ "Could not get settings, make sure to run 'ghup config' first"
            )
            pure
          $ getSettings
      (flip runReaderT) settings . runExceptT . withExceptT show $ e
  e <-
    customExecParser (prefs showHelpOnError) (info (opts <**> helper) idm)
      >>= \case

    -- fork
            Fork (ForkOptions {..}) -> run $ do
              case repoBasePath of
                Just rbp -> case parseAbs rbp of
                  Just p -> prepareRepoForPR' repo (Just p) newBranch
                  Nothing ->
                    liftIO $ die (color Red $ "Repo path must be absolute")
                Nothing -> prepareRepoForPR' repo Nothing newBranch

            -- config
            Config (ConfigOptions {..}) -> do
              p <- maybe (pure Nothing) (fmap Just . parseAbs) bPath
              writeSettings (Settings (OAuth oAuth) p) <&> Right

            -- delete
            Del       (DelOptions {..}     ) -> run $ deleteFork' del

            -- list-forks
            ListForks (ListForkOptions {..}) -> run $ do
              mtime <- parseSince lSince
              forks <- withExceptT show $ getForks mtime
              let
                formatted =
                  gridString
                      [column expand left def def, column expand left def def]
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

            -- gistc
            CreateGist (CreateGistOptions {..}) -> run $ do
              let desc   = maybe T.empty E.decodeUtf8 description
                  public = not private
              gist <- case input of
                StdInput        -> postGistStdin desc public
                FileInput files -> do
                  files' <- for files $ \file -> do
                    let absPath = parseAbs file
                    let relPath = parseRel file
                    case (absPath, relPath) of
                      (Just a, _) -> pure $ AnyPath $ a
                      (_, Just a) -> pure $ AnyPath $ a
                      _ -> throwError (uError "Could not parse path")

                  postGistFiles files' desc public
              liftIO $ putStrLn $ T.unpack $ getUrl $ gistHtmlUrl gist

            -- gistl
            ListGist (ListGistOptions {..}) -> run $ do
              mtime <- parseSince lgSince
              gists <- listGists mtime
              let
                formatted =
                  gridString
                      (  [column expand left def def]
                      <> (if lgDesc then [column expand left def def] else [])
                      <> [column expand left def def]
                      )
                    $ fmap
                        (\Gist {..} ->
                          [(T.unpack . getUrl $ gistHtmlUrl)]
                            <> (if lgDesc
                                 then
                                   [ T.unpack $ fromMaybe (T.pack "(No desc)")
                                                          gistDescription
                                   ]
                                 else []
                               )
                            <> [ formatShow (iso8601Format :: Format Day)
                                            (utctDay gistUpdatedAt)
                               ]
                        )
                        gists
              liftIO $ putStrLn $ formatted



  -- print error, if any
  case e of
    Right () -> pure ()
    Left  t  -> die (color Red $ t)

 where
  parseSince lSince = do
    liftIO $ case lSince of
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
