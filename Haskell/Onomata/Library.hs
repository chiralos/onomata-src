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

module Onomata.Library ( 
  defaultEnv
  ) where

import Onomata.CoreLib
import Onomata.DynLib
import Onomata.Types

-- see [1]
defaultEnv :: Env
defaultEnv = overwriteEnv defaultLib emptyEnv

overwriteEnv :: Library -> Env -> Env
overwriteEnv lib env = env' where
  env' = foldl (flip addBinding) env lib

defaultLib :: Library
defaultLib = coreLib ++ dynLib

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
