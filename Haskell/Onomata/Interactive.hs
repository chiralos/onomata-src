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
module Onomata.Interactive ( 
  repl
  ) where

import Control.Exception
import System.Console.Haskeline.IO -- see [1]
import System.Console.Haskeline

import Onomata.Interpreter
import Onomata.Library
import Onomata.Parser
import Onomata.Types

repl :: IO ()
repl = launch where
  launch = bracketOnError 
    (initializeInput defaultSettings)
    cancelInput
    (\hd -> start hd >> closeInput hd)
  start hd = loop ([],defaultEnv) where
    loop st = do
      mln <- queryInput hd (getInputLine "ono> ")
      case mln of
        Nothing -> return ()
        Just ln -> do
          let
            erf msg = err ("parse: " ++ msg) st
            runf p  = interpret p st
          (stk',e') <- either erf runf $ parseProg "(interactive)" ln
          let
            continue = do
              stackToConsole stk'
              loop (stk',e')
          case status e' of
            OK        -> continue
            Exit      -> return ()
            Error erm -> do
              queryInput hd (outputStrLn ("error: " ++ toString erm))
              continue
    stackToConsole stk = 
      queryInput hd (outputStrLn ("| " ++ printStack stk))

--------
-- Notes
{-
  [1] This seem heavyweight, could replace with lower level ANSI
      terminal codes and basic termcap check.
-}
