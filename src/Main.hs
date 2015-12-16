import System.Environment

import Language.Haskell.GhcOpts

main :: IO () 
main = do
  f:_ <- getArgs
  z   <- ghcOpts f
  putStrLn $ "GHC Options: " ++ show z
