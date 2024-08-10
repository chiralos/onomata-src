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

module Onomata.Interpreter ( 
  interpret
  ) where

import Onomata.Types

interpret :: Exp -> Program

interpret (Quo p) (stk,env) = ok (p : stk,env)
interpret (Prm f) st        = f st
interpret (Sym s) (stk,env) = case consult env s of
  Nothing -> err (toString s <> ": unknown word") (stk,env)
  Just f  -> interpret f (stk,env)
interpret (Seq [])     st   = ok st
interpret (Seq [p])    st   = interpret p st -- tail call
interpret (Seq (p:ps)) st   = do
  (stk',env') <- interpret p st
  case status env' of
    OK        -> interpret (Seq ps) (stk',env')
    Error msg -> err (toString msg) (stk',env')
    Exit      -> ok (stk',env')
-- assume everything else is a data object 
-- if we aren't doing the data-push thing, error here
interpret x (stk,env) = ok (x : stk,env)
