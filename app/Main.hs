module Main where

import           Control.Monad
import           Data.ByteString                ( ByteString )
import           Data.Functor                   ( (<&>) )
import           Data.Semigroup                 ( (<>) )
import           GHup
import           GitHub.Auth
import           HPath
import           Options.Applicative
import           System.Console.Pretty
import           System.Exit



data Options = Options
  { optCommand :: Command
  }

data Command
  = Fork ForkOptions
  | Config ConfigOptions
  | Del DelOptions

data ForkOptions = ForkOptions
  {
    repo          :: ByteString
  , newBranch     :: Maybe ByteString
  , repoBasePath  :: Maybe ByteString
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
  (  command "fork"   (Fork <$> (info (forkOpts <**> helper) idm))
  <> command "config" (Config <$> (info (configOpts <**> helper) idm))
  <> command "delete" (Del <$> (info (delOpts <**> helper) idm))
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


main :: IO ()
main = do
  e <- execParser (info (opts <**> helper) idm) >>= \case
    Fork (ForkOptions {..}) -> do
      case repoBasePath of
        Just rbp -> case parseAbs rbp of
          Just p  -> prepareRepoForPR' repo (Just p) newBranch
          Nothing -> fail "Repo path must be absolute"
        Nothing -> prepareRepoForPR' repo Nothing newBranch
    Config (ConfigOptions {..}) -> do
      p <- maybe (pure Nothing) (fmap Just . parseAbs) bPath
      writeSettings (Settings (OAuth oAuth) p) <&> Right
    Del (DelOptions {..}) -> deleteFork' del
  case e of
    Right () -> _info "success!"
    Left  t  -> die (color Red $ t)
