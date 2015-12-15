{-# LANGUAGE CPP #-}

-- | This module adds support for `stack`, as follows:
--   1. Figure out if the target-file is in a stack project,
--   2. If `stack.yaml` in PATH, run `stack exec` to extract `StackConfig`
--   3. Use `StackConfig` to alter the cabal ConfigFlags in Cabal.hs

module Language.Haskell.GhcOpts.Stack ( getStackConfig ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative((<$>), (<*>))
import System.IO
#endif

import Data.Maybe (listToMaybe)

import System.FilePath
import System.Directory
import Control.Monad (filterM)

import Language.Haskell.GhcOpts.Utils
import Language.Haskell.GhcOpts.Types

--------------------------------------------------------------------------------
getStackConfig :: Maybe FilePath -> IO (Maybe StackConfig)
--------------------------------------------------------------------------------
getStackConfig Nothing  =
  return Nothing
getStackConfig (Just p) = do
  mbYaml <- getStackYaml p
  case mbYaml of
    Nothing -> return Nothing
    Just _  -> do mdbs <- getStackDbs p
                  mdst <- getStackDist p
                  return $ StackConfig <$> mdst <*> mdbs

--------------------------------------------------------------------------------
getStackYaml :: FilePath -> IO (Maybe FilePath)
--------------------------------------------------------------------------------
getStackYaml p = listToMaybe <$> filterM doesFileExist paths
  where
    paths      = [ d </> "stack.yaml" | d <- pathsToRoot dir]
    dir        = takeDirectory p

--------------------------------------------------------------------------------
getStackDist :: FilePath -> IO (Maybe FilePath)
--------------------------------------------------------------------------------
getStackDist p = (trim <$>) <$> execInPath cmd p
  where
    cmd        = "stack path --dist-dir"

--------------------------------------------------------------------------------
getStackDbs :: FilePath -> IO (Maybe [FilePath])
--------------------------------------------------------------------------------
getStackDbs p = do mpp <- execInPath cmd p
                   case mpp of
                       Just pp -> Just <$> extractDbs pp
                       Nothing -> return Nothing
  where
    cmd       = "stack --verbosity quiet exec printenv GHC_PACKAGE_PATH"

extractDbs :: String -> IO [FilePath]
extractDbs = filterM doesDirectoryExist . stringPaths

stringPaths :: String -> [String]
stringPaths = splitBy ':' . trim
