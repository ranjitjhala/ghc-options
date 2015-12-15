import System.Environment

import Language.Haskell.GhcOptions

main :: IO ()
main = do 
  f:_ <- getArgs
  z   <- ghcOpts z
  putStrLn $ "GHC Options: " ++ show z
