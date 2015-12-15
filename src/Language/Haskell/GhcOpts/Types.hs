module Language.Haskell.GhcOpts.Types (
    CmdExtra    (..)
  , Config      (..)
  , CabalConfig (..)
  , StackConfig (..)
  ) where

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
