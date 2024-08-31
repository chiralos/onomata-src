{-
        / \
      /     \
     |\     / \ / \  chiral software
     |  \ /     \   \
      \  |\     /|  /|
        \|  \ /  |/  |
         |\  |  /|  /
         |  \|/  |/
          \  |  /
            \|/
-}
module Onomata.Main( 
  main
  ) where

import System.Environment (getArgs)
import System.Exit

import Onomata.Interactive
import Onomata.Interpreter
import Onomata.Library
import Onomata.Parser
import Onomata.Types

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then onoi
    else batch ([],defaultEnv) args
  exitWith ExitSuccess

batch :: State -> [String] -> IO ()
batch _ [] = exitWith ExitSuccess
batch (stk,env) (fn:fns) = do
  f <- readFile fn
  case parseProg fn f of
    Left msg -> die ("parse error: " ++ msg)
    Right p  -> do
      (stk',env') <- interpret p (stk,env)
      case (status env') of
        OK -> batch (stk',env') fns
        Exit -> exitWith ExitSuccess
        Error msg -> die ("error (" ++ fn ++ "): " ++ toString msg)

--------
-- Notes

{-

-}
