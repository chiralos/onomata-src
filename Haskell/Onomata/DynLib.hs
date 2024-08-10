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

module Onomata.DynLib ( 
  dynLib
  ) where

import Data.Dict

import Onomata.LibraryTypes
import Onomata.Parser
import Onomata.Types

dynLib = mkLib [
  ("def",     Prm primDef),
  ("parse",   Prm primParse),
  ("unparse", Prm primUnparse),
  ("quit",    Prm (\(stk,env) -> return (stk,setExit env)))
  ]

primDef (Str wName : q : stk, env) = res where
  res = ok (stk, addBinding (wName,q) env)
primDef st = err ("def: expected: val str") st

primParse st@(Str s : ps, env) = 
  case parseExp "(interactive)" $ toString s of
    Left  erm  -> err ("parse: " ++ erm) st
    Right expr -> ok (expr : ps,env)
primParse st = err ("parse: expected: str") st

primUnparse (p : ps, env) = 
  ok ((Str $ fromString $ show p) : ps, env)
primUnparse st = underflow "unparse" st

-------
-- Misc

fi = fromInteger

--------
-- Notes
{-
  [1] Library structure has not been worked out: defaultLib is
      just a bag of basic stuff.

  hostLib = mkLib [
    ("load", Prm primLoad)
    ]

  primLoad :: Program
  primLoad (Str libName : stk,env) = res where
    res = case look builtinLibs libName of
      Nothing    -> err ("unknown library: " ++ 
                         toString libName) (stk,env)
      Just entry -> ok (stk,env') where
        env' = overwriteEnv (snd entry) env
  primLoad st = err ("load: expected: str") st

builtinLibs :: Dict Bytes Library
builtinLibs = newDict [
  (fromString "std/core", coreLib),
  (fromString "std/cat",  catLib)
  ]

-}
