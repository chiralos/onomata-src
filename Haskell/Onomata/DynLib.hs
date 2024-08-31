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

import Control.Monad (when)
import Data.List (partition)

import Data.Dict

import Onomata.LibraryTypes
import Onomata.Parser
import Onomata.Types

dynLib = mkLib [
  ("def",     Prm primDef),

  ("clear", Prm (\(stk,env) -> ok ([],env))),
  -- reset is handled in Library module
  ("list",  Prm primList),
  ("show",  Prm primShow),
  ("undef", Prm primUndef)
  ]

primDef (q : Str wName : stk, env) = res where
  res = ok (stk, addBinding (wName,(q,True)) env)
primDef st = err ("def: expected: val str") st

maxLineLen = 40

primList (ps,env) = do
  let 
    f n [] = when (n > 0) (putStr "\n")
    f n (w:wds) = do
      n' <- if n > maxLineLen
        then putStr "\n" >> return 0
        else return (n + length w)
      putStr w
      putStr " "
      f n' wds
    (user,builtin) = 
      pairmap (map (toString . fst)) $
      partition (snd . snd)          $
      listDict                       $ 
      bindings env
  f 0 builtin
  when (not $ null user) $ putStrLn "-- user --"
  f 0 user
  ok (ps,env)

primShow st@(Str s : ps,env) = do
  case consult env s of
    Nothing -> err "unknown symbol" st
    Just e  -> do
      putStrLn $ show e
      ok (ps,env)
primShow st = err ("show: expected Str") st

primUndef st@(Str s : ps,env) = do
  case consult env s of
    Nothing -> err "unknown symbol" st
    Just e  -> ok (ps, Env { 
      status = OK,
      bindings = bindings env `del` s })
primUndef st = err ("undef: expected Str") st

-------
-- Misc

pairmap :: (a -> b) -> (a,a) -> (b,b)
pairmap f (x,y) = (f x, f y)

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
