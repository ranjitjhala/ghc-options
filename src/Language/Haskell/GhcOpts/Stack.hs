{-# LANGUAGE CPP #-}
module Stack
      ( -- * The bits of information needed from `stack`
        StackConfig (..)
        -- * Run `stack exec` to compute @StackConfig@
      , getStackConfig
      ) where

import Data.Maybe (listToMaybe)
import Data.Char (isSpace)

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative((<$>), (<*>))
import System.IO
#endif

import System.Process
import System.FilePath
import System.Directory
import Control.Monad (filterM)
import Control.Exception


-- | This module adds support for `stack`, as follows:
--   1. Figure out if the target-file is in a stack project,
--   2. If `stack.yaml` in PATH, run `stack exec` to extract `StackConfig`
--   3. Use `StackConfig` to alter the cabal ConfigFlags in Cabal.hs


-- TODO: Move into Types?
data StackConfig = StackConfig { stackDist :: FilePath
                               , stackDbs  :: [FilePath]
                               }
                   deriving (Eq, Show)

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

pathsToRoot :: FilePath -> [FilePath]
pathsToRoot p
  | p == parent = [p]
  | otherwise   = p : pathsToRoot parent
  where
    parent      = takeDirectory p

--------------------------------------------------------------------------------
getStackDist :: FilePath -> IO (Maybe FilePath)
--------------------------------------------------------------------------------
getStackDist p = (trim <$>) <$> execInPath cmd p
  where
    cmd        = "stack path --dist-dir"
    -- dir        = takeDirectory p
    -- splice     = (dir </>) . trim

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

--------------------------------------------------------------------------------
-- | Generic Helpers
--------------------------------------------------------------------------------

splitBy :: Char -> String -> [String]
splitBy c str
  | null str' = [x]
  | otherwise = x : splitBy c (tail str')
  where
    (x, str') = span (c /=) str

trim :: String -> String
trim = f . f
   where
     f = reverse . dropWhile isSpace

#if __GLASGOW_HASKELL__ < 709
execInPath :: String -> FilePath -> IO (Maybe String)
execInPath cmd p = do
    eIOEstr <- try $ createProcess prc :: IO (Either IOError ProcH)
    case eIOEstr of
        Right (_, Just h, _, _)  -> Just <$> getClose h
        Right (_, Nothing, _, _) -> return Nothing
        -- This error is most likely "/bin/sh: stack: command not found"
        -- which is caused by the package containing a stack.yaml file but
        -- no stack command is in the PATH.
        Left _  -> return Nothing
  where
    prc          = (shell cmd) { cwd = Just $ takeDirectory p }

getClose :: Handle -> IO String
getClose h = do
  str <- hGetContents h
  hClose h
  return str

type ProcH = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

-- Not deleting this because this is likely more robust than the above! (but
-- only works on process-1.2.3.0 onwards

#else
execInPath :: String -> FilePath -> IO (Maybe String)
execInPath cmd p = do
    eIOEstr <- try $ readCreateProcess prc "" :: IO (Either IOError String)
    return $ case eIOEstr of
        Right s -> Just s
        -- This error is most likely "/bin/sh: stack: command not found"
        -- which is caused by the package containing a stack.yaml file but
        -- no stack command is in the PATH.
        Left _  -> Nothing
  where
    prc          = (shell cmd) { cwd = Just $ takeDirectory p }
#endif
