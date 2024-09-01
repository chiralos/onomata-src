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

module Onomata.CommonLib ( 
  commonLib
  ) where

import Data.Bits
import Data.Char
import qualified Data.ByteArray as B

import Data.Dict

import Onomata.Interpreter
import Onomata.LibraryTypes
import Onomata.Parser
import Onomata.Types

commonLib :: Library
commonLib = concat [
  stackLib, controlLib,
  boolLib, ordLib, intLib, bitLib,
  seqLib,
  commonTypesLib, rawTupLib,
  bytesLib, parseLib, sysLib]

stackLib :: Library
stackLib = mkLib $ map mkStackCmd [
  ("nop", 0, id),
  ("pop", 1, tail),
  ("dup", 1, \(p:ps)     -> p:p:ps),
  ("swp", 2, \(p:q:ps)   -> q:p:ps),
  ("swu", 3, \(c:b:a:ps) -> c:a:b:ps),
  ("swa", 3, \(c:b:a:ps) -> a:b:c:ps),
  ("bry", 3, \(c:b:a:ps) -> b:a:c:ps),
  ("dig", 3, \(c:b:a:ps) -> a:c:b:ps)
  ]

controlLib :: Library
controlLib = mkLib [
  mkStackCmd ("quo", 1, \(p:ps) -> Quo p : ps),
  mkStackCmd ("cat", 2, \(p:q:ps) -> Seq [q,p] : ps),
  ("run",  Prm primRun),
  ("dip",  Prm primDip),
  ("loop", loopPrecomp),
  ("ife",  Prm primIfe)
  ]

primRun (p:ps,env) = interpret p (ps,env)
primRun st         = underflow "run" st

primDip (p:q:ps,env) = do
  (ps',env') <- interpret p (ps,env)
  return (q:ps',env')
primDip st = underflow "dip" st

loopPrecomp = parseStatic "dup dip swp (loop) (pop) ife"

primIfe :: Program
primIfe st@(stk,env)
  | length stk < 3 = underflow "choose" st
  | p : q : Bit True  : ps <- stk = interpret q (ps,env)
  | p : q : Bit False : ps <- stk = interpret p (ps,env)
  | otherwise                     = eRet
  where
    eRet = err "ife: expected: Bool (A) (A)" st

boolLib :: Library
boolLib = mkLib [
  ("true",   Bit True),
  ("false",  Bit False),
  ("and",    Prm $ boolPrim "and" (&&)),
  ("or",     Prm $ boolPrim "or"  (||)),
  ("not",    Prm   primNot),
  ("choose", Prm   primChoose)
  ]

boolPrim nm op (Bit b : Bit a : ps, env) =
  ok (Bit (a `op` b) : ps,env)
boolPrim nm op st@(stk,env)
  | length stk < 2 = underflow nm st
  | otherwise      = err (nm ++ ": expected: Bool Bool") st

primNot :: Program
primNot st@(stk,env)
  | null stk          = underflow "not" st
  | Bit b : ps <- stk = ok ((Bit $ not b):ps,env)
  | otherwise         = eRet 
  where
    eRet = err "not: expected: Bool" st

primChoose :: Program
primChoose st@(stk,env)
  | length stk < 3 = underflow "choose" st
  | Bit True  : p : q : ps <- stk = ok (q:ps,env)
  | Bit False : p : q : ps <- stk = ok (p:ps,env)
  | otherwise                     = eRet
  where
    eRet = err "choose: expected: (A) (A) Bool" st
    
intLib :: Library
intLib = mkLib [
  ("add",    Prm $ binMathPrim False "add" (+)),
  ("sub",    Prm $ binMathPrim False "sub" (-)),
  ("mul",    Prm $ binMathPrim False "mul" (*)),
  ("div",    Prm $ binMathPrim True  "div" div),
  ("mod",    Prm $ binMathPrim True  "mod" mod),

  ("divmod", Prm $ primDivmod),
  ("neg",    Prm $ unaryMathPrim "neg" negate),
  ("abs",    Prm $ unaryMathPrim "abs" abs)
  ]

binMathPrim dc nm op st@(Num b : Num a : ps, env)
  | dc && b == 0 = err (nm ++ ": divide by zero") st
  | otherwise    = ok (Num (a `op` b) : ps,env)
binMathPrim _ nm _ st@(stk,env)
  | length stk < 2 = underflow nm st
  | otherwise      = err (nm ++ ": expected Num Num") st

primDivmod st@(Num b : Num a : ps, env)
  | b == 0    = err ("divmod: divide by zero") st
  | otherwise = ok (r:q:ps,env)
  where
    r = Num (a `mod` b)
    q = Num (a `div` b)
primDivmod st@(stk,env)
  | length stk < 2 = underflow "divmod" st
  | otherwise      = err ("divmod: expected Num Num") st

unaryMathPrim nm op st@(Num a : ps, env) = ok (Num (op a) : ps, env)
unaryMathPrim nm _  st = err (nm ++ ": expected Num") st

bitLib :: Library
bitLib = mkLib [
  ("bit/and",   Prm $ binMathPrim False "bit/and" (.&.)),
  ("bit/or",    Prm $ binMathPrim False "bit/or"  (.|.)),
  ("bit/xor",   Prm $ binMathPrim False "bit/xor" xor),
  ("bit/not",   Prm $ unaryMathPrim "neg" complement),
  ("bit/shift", Prm $ binMathPrim False "bit/shift" shift)
  ]

ordLib :: Library
ordLib = mkLib [
  ("lt",  Prm $ ordPrim "lt"  (<)),
  ("lte", Prm $ ordPrim "lte" (<=)),
  ("gt",  Prm $ ordPrim "gt"  (>)),
  ("gte", Prm $ ordPrim "gte" (<=)),
  ("eq",  Prm $ ordPrim "eq"  (==)),
  ("neq", Prm $ ordPrim "neq" (/=))
  ]

ordPrim nm op st@(ea : eb : ps, env)
  | Bit a <- ea, Bit b <- eb  = okRet
  | Num a <- ea, Num b <- eb  = okRet
  | Str a <- ea, Str b <- eb  = okRet
  | otherwise                 = err (nm ++ ": type mismtach") st
  where
    okRet = ok (Bit (ea `op` eb) : ps,env)

ordPrim nm _ st = underflow nm st

commonTypesLib :: Library
commonTypesLib = mkLib [
  ("unit",   Prm primUnit),
  ("pair",   Prm primPair),
  ("fst",    Prm primFst),
  ("snd",    Prm primSnd),
  ("unpair", Prm primUnpair),
  ("left",   Prm primLeft),
  ("right",  Prm primRight),
  ("alt",    Prm primAlt),
  ("val",    Prm primVal),
  ("nil",    Prm primNil),
  ("opt",    Prm primOpt)
  ]

primUnit (ps,env) = ok (Tup [] : ps,env)

primPair (p:q:ps,env) = ok (Tup [q,p] : ps,env)
primPair st = underflow "pair" st

primFst (Tup [a,b] : ps,env) = ok (a : ps,env)
primFst st = err ("fst: expected: Pair") st

primSnd (Tup [a,b] : ps,env) = ok (b : ps,env)
primSnd st = err ("snd: expected: Pair") st

primUnpair (Tup [a,b] : ps,env) = ok (b : a : ps,env)
primUnpair st = err ("unpair: expected: Pair") st

primLeft (p:ps,env) = ok (Tup [Bit False,p] : ps,env)
primLeft st = underflow "left" st

primRight (p:ps,env) = ok (Tup [Bit True,p] : ps,env)
primRight st = underflow "right" st

primAlt (_:l:Tup [Bit False,lv]:ps,env) = interpret l (lv:ps,env)
primAlt (r:_:Tup [Bit True, rv]:ps,env) = interpret r (rv:ps,env)
primAlt st = err ("alt: expected Alt Proc Proc") st

primNil (ps,env) = ok (Tup [Bit False] : ps,env)

primVal (p:ps,env) = ok (Tup [Bit True,p] : ps,env)
primVal st = underflow "val" st

primOpt (_:d:Tup (Bit False:_):ps,env) = ok (d : ps,env)
primOpt (r:_:Tup [Bit True, v]:ps,env) = interpret r (v:ps,env)
primOpt st = err ("opt: expected Opt (D) Proc") st

rawTupLib :: Library
rawTupLib = mkLib [
  ("pack",    Prm primPack),
  ("unpack",  Prm primUnpack),
  ("tup/get", Prm primTupGet),
  ("tup/set", Prm primTupSet)
  ]

primPack st@(Num n:ps,env)
  | n < 0         = err "pack: negative size" st
  | length ps < n = underflow "pack" st
  | otherwise     = ok (Tup (reverse tc):rest,env)
  where
    (tc,rest) = splitAt n ps

primUnpack (Tup t:ps,env) = ok (reverse t ++ ps,env)

primTupGet st@(Num n:Tup t:ps,env)
  | n < 0        = err "tup/get: negative index" st
  | length t < n = err "tup/get: bounds violation" st
  | otherwise    = ok ((t !! n):ps,env)
primTupGet st = underflow "tup/get" st

primTupSet st@(Num n:x:Tup t:ps,env)
  | n < 0        = err "tup/set: negative index" st
  | length t < n = err "tup/set: bounds violation" st
  | otherwise    = ok (Tup (tc ++ (x:tail rest)):ps,env)
  where
    (tc,rest) = splitAt n t
primTupSet st = underflow "tup/set" st
    
seqLib :: Library
seqLib = mkLib [
  ("len", Prm primLen),
  ("sbs", Prm primSubstr), -- subsequence
  ("slc", Prm primSlice)   -- slice
  ]

primLen (Str s : ps,e) = ok ((Num $ B.length s) : ps,e)
primLen (Tup t : ps,e) = ok ((Num $ length t) : ps,e)
primLen st = err ("len: expected: Seq") st

primSubstr
  (Num n : Num m : Str s : ps,e) = r where
    n' = if n == -1 then B.length s else n
    s' = B.take n' $ B.drop m s
    r  = ok (Str s':ps,e)
primSubstr st = err ("sbs: expected: Seq Int Int") st

primSlice 
  (Num n : Num m : Str s : ps,e) = r where
    n' = if n == -1 then B.length s else n
    s' = B.take (n'-m) $ B.drop m s
    r  = ok (Str s':ps,e)
primSlice st = err ("slc: expected: seq int int") st

bytesLib :: Library
bytesLib = mkLib [
  ("chr",      Prm primChr),
  ("str",      Prm primStr),
  ("str/cat",  Prm primStrCat),
  ("str/get",  Prm primStrGet),
  ("str/set",  Prm primStrSet)
  ]

primChr st@(Num n : ps,e)
  | n < 256   = ok (Str (singleByte n) : ps,e)
  | otherwise = err ("chr: int > 255") st
primChr st = err ("chr: expected: Int") st

singleByte :: (Integral a, B.ByteArray b) => a -> b
singleByte = B.singleton . fromIntegral

primStr s@(Str b : ps,e) = ok s
primStr (Bit True : ps,e) = ok (Str (fromString "true") : ps,e)
primStr (Bit False : ps,e) = ok (Str (fromString "false") : ps,e)
primStr (Num n : ps,e) = ok (Str ns : ps,e) where
  ns = fromString $ show n

primStrCat (Str b : Str a : ps,env) = ok (Str (a<>b) : ps,env)
primStrCat st = err ("str/cat: expected: seq seq") st

primStrGet st@(Num n:Str s:ps,e)
  | n < 0          = err "str/get: negative index" st
  | B.length s < n = err "str/get: bounds violation" st
  | otherwise      = ok (Num (fromIntegral (s `B.index` n)):ps,e)
primStrGet st = underflow "str/get" st

primStrSet st@(Num n:Num x:Str s:ps,env)
  | n < 0          = err "str/set: negative index" st
  | B.length s < n = err "str/set: bounds violation" st
  | otherwise      = ok (Str s' : ps,env)
  where
    s1 = B.take n s
    s2 = B.drop (n+1) s
    s' = s1 <> singleByte x <> s2
primStrSet st = underflow "str/set" st

-- see [1]
parseLib :: Library
parseLib = mkLib [
  ("parse",   Prm primParse),
  ("unparse", Prm primUnparse)
  ]

primParse st@(Str s : ps, env) = 
  case parseExp "(interactive)" $ toString s of
    Left  erm  -> err ("parse: " ++ erm) st
    Right expr -> ok (expr : ps,env)
primParse st = err ("parse: expected: str") st

primUnparse (p : ps, env) = 
  ok ((Str $ fromString $ show p) : ps, env)
primUnparse st = underflow "unparse" st

sysLib :: Library
sysLib = mkLib [
  ("exit",    Prm (\(stk,env) -> return (stk,setExit env))),
  ("wordsize", Prm primWordsize)
  ]

primWordsize (ps,env) = ok (Num ws : ps,env) where
  ws = finiteBitSize (0 :: Word) `div` 8

--------
-- Notes
{-
[1] Experimental. 'parse' should probably parse a sequence and
    always return a proc.
-}
