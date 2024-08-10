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

module Onomata.LibraryTypes ( 
  StackCmd, mkStackCmd, underflow, mkLib
  ) where

import Onomata.Types

type StackCmd = (String,Int,Stack -> Stack)

mkStackCmd :: StackCmd -> (String,Exp)
mkStackCmd (nm,ari,sf) = (nm, Prm f) where
  f (stk,env)
    | length stk < ari = underflow nm (stk,env)
    | otherwise        = ok (sf stk,env)

underflow :: String -> State -> IO State
underflow nm = err (nm ++ ": stack underflow")

mkLib :: [(String,Exp)] -> Library
mkLib = map (fstMap fromString)

-------
-- Misc

fstMap :: (a -> b) -> (a,c) -> (b,c)
fstMap f (x,y) = (f x,y)
