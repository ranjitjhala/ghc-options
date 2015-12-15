module Language.Haskell.GhcOpts 
  ( ghcOpts
  , module Language.Haskell.GhcOpts.Types
  ) where

import Language.Haskell.GhcOpts.Types

ghcOpts :: CommandExtra -> IO Config
ghcOpts = undefined

