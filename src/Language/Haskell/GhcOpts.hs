module Language.Haskell.GhcOpts
  ( ghcOpts
  , module Language.Haskell.GhcOpts.Types
  ) where

import System.Directory (setCurrentDirectory)
import System.FilePath  (takeDirectory)

import Language.Haskell.GhcOpts.Types
import Language.Haskell.GhcOpts.Cabal
import Language.Haskell.GhcOpts.Stack

--------------------------------------------------------------------------------
ghcOpts :: CommandExtra -> IO (Either String Config)
--------------------------------------------------------------------------------
ghcOpts cmd = newConfig cmd >>= packageConfig

newConfig :: CommandExtra -> IO Config
newConfig cmd = do
    cabal <- traverse (\path -> mkCabalConfig path (ceCabalOptions cmd)) $ ceCabalConfig cmd
    stack <- getStackConfig (cePath cmd)
    return Config { configGhcOpts = "-O0" : ceGhcOptions cmd
                  , configCabal   = cabal
                  , configStack   = stack }

packageConfig :: Config -> IO (Either String Config)
packageConfig cfg =
  case configCabal cfg of
    Nothing -> return $ Right cfg
    Just c  -> (withOpts cfg <$>) <$> packageOptions (configStack cfg) c

withOpts :: Config -> [String] -> Config
withOpts cfg opts = cfg { configGhcOpts = opts ++ configGhcOpts cfg }

packageOptions :: Maybe StackConfig -> CabalConfig -> IO (Either String [String])
packageOptions stack cabal = do
  setCurrentDirectory . takeDirectory $ cabalConfigPath cabal
  getPackageGhcOpts (cabalConfigPath cabal) stack (cabalConfigOpts cabal)
