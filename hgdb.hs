-- Test of the hgdbmi package
module Main (main) where

import GDBMI
import System.Environment (getArgs)
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  pid <- return . read . head $ args
  connAttempt <- attach Nothing pid
  case connAttempt of
    Left err          -> print err
    Right (conn, res) -> ghcInterp conn (Right res)

ghcInterp :: GDB -> Either String MIOutput -> IO ()
ghcInterp conn res = do
  ghcShowResult res
  input <- getLine
  case input of
    ":quit" -> return ()
    cmd -> do res' <- runCommand (CLICommand cmd) conn
              ghcInterp conn res'

ghcShowResult :: Either String MIOutput -> IO ()
ghcShowResult (Left s) = print s
ghcShowResult (Right (MIOutput os res)) = do
  putStr $ showOOBs os
  print res
    where
      showOOBs (MIStatus s:os)  = "Status: " ++ (ensureNL s) ++ showOOBs os
      showOOBs (MIExec s:os)    = "Exec: " ++ (ensureNL s) ++ showOOBs os
      showOOBs (MINotify s:os)  = "Notify: " ++ (ensureNL s) ++ showOOBs os
      showOOBs (MIConsole s:os) = "Console: " ++ (ensureNL s) ++ showOOBs os
      showOOBs (MITarget s:os)  = "Target: " ++ (ensureNL s) ++ showOOBs os
      showOOBs (MILog s:os)     = "Log: " ++ (ensureNL s) ++ showOOBs os
      showOOBs []               = ""

ensureNL :: String -> String
ensureNL ('\n':[]) = "\n"
ensureNL (c:[]) = c:"\n"
ensureNL (c:cs) = c:(ensureNL cs)
