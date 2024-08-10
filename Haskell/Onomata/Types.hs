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

module Onomata.Types ( 
  Exp(..), 
  Stack, State, emptyState,
  Env(..), emptyEnv, consult, addBinding,
  Status(..), setExit,
  Program, err, ok,
  Library,
  Bytes, fromString, toString
  ) where

import Data.Char (chr,ord)
import Data.IORef
import qualified Data.ByteArray as B

import Data.Dict

type Bytes = B.Bytes


data Exp =
    Bit Bool
  | Num Int
  | Str Bytes
  | Vec [Exp]
  | Ref (IORef Exp)

  | Sym Bytes

  | Seq [Exp]
  | Quo Exp
  | Prm Program

instance Eq Exp where
  Bit a == Bit b = a == b
  Num a == Num b = a == b
  Str a == Str b = a == b
  _     == _     = error "unsupported Eq"

instance Ord Exp where
  Bit a <= Bit b = a <= b
  Num a <= Num b = a <= b
  Str a <= Str b = a <= b
  _     <= _     = error "unsupported Ord"

data Env = Env {
  bindings :: Dict Bytes Exp,
  status   :: Status }

addBinding :: (Bytes,Exp) -> Env -> Env 
addBinding entry env = env {
  bindings = bindings env `put` entry }

data Status = OK | Error Bytes | Exit

setStatus :: Status -> Env -> Env
setStatus s env = env { status = s }

setError :: Bytes -> Env -> Env
setError erm = setStatus (Error erm)

setExit, setOK :: Env -> Env
setExit = setStatus Exit
setOK   = setStatus OK

type State   = (Stack, Env) -- see [1]
type Stack   = [Exp]
type Program = State -> IO State
type Library = [(Bytes,Exp)]

emptyState :: State
emptyState = ([],emptyEnv)

emptyEnv :: Env
emptyEnv = Env {
  bindings = empty,
  status   = OK }

consult :: Env -> Bytes -> Maybe Exp
consult env = (fmap snd) . look (bindings env)

err :: String -> State -> IO State
err msg (s,env) = return (s,setError (fromString msg) env)

ok :: (Stack,Env) -> IO State
ok (s,env) = return (s,setOK env)

-- HERE 
toString :: Bytes -> String
toString = map (chr . fromIntegral) . B.unpack

fromString :: String -> Bytes
fromString = B.pack . map (fromIntegral . ord)

--------
-- Notes

{-
  [1] We deliberate do not make this (Prog,Stack,Env) to
      emphasise that programs do _not_ have access to the
  future of the computation. This is not Forth.

  ... maybe internally we do represent it that way, to
  make run and dip nicer ?
-}
