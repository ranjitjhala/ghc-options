module Language.Haskell.GhcOpts.Types (
    CommandExtra (..)
  , Config       (..)
  , CabalConfig  (..)
  , StackConfig  (..)
  ) where

import System.Posix.Types (EpochTime)

data Config = Config
  { configGhcOpts :: [String]
  , configCabal   :: Maybe CabalConfig
  , configStack   :: Maybe StackConfig
  }
  deriving (Eq, Show)

data CabalConfig = CabalConfig
  { cabalConfigPath :: FilePath
  , cabalConfigOpts :: [String]
  , cabalConfigLastUpdatedAt :: EpochTime
  }
  deriving (Eq, Show)

data StackConfig = StackConfig
  { stackDist :: FilePath
  , stackDbs  :: [FilePath]
  }
  deriving (Eq, Show)

-- | Bonus config parameters used to override those in .cabal and stack.yaml

data CommandExtra = CommandExtra
  { ceGhcOptions   :: [String]
  , ceCabalConfig  :: Maybe FilePath
  , cePath         :: Maybe FilePath
  , ceCabalOptions :: [String]
  } deriving (Read, Show)
