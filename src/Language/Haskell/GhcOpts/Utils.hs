{-# LANGUAGE CPP #-}

-- | Various utilities pertaining to searching for files & directories.

module Language.Haskell.GhcOpts.Utils where

import Control.Exception
import System.Process
import Data.Char (isSpace)
import System.FilePath
import System.Directory


getDirectoryContentsIfExists :: FilePath -> IO [FilePath]
getDirectoryContentsIfExists dir = do
  b <- doesFileExist dir
  if b then getDirectoryContents dir
       else return []

absoluteFilePath :: FilePath -> IO FilePath
absoluteFilePath p = if isAbsolute p then return p else do
    dir <- getCurrentDirectory
    return $ dir </> p


pathsToRoot :: FilePath -> [FilePath]
pathsToRoot p
  | p == parent = [p]
  | otherwise   = p : pathsToRoot parent
  where
    parent      = takeDirectory p

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
