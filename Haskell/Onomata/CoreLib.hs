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

module Onomata.CoreLib ( 
  coreLib
  ) where

import Data.Char
import qualified Data.ByteArray as B

import Data.Dict

import Onomata.Interpreter
import Onomata.LibraryTypes
import Onomata.Types

coreLib :: Library
coreLib = concat [
  stackLib, runLib, higherOrderLib, 
  boolLib, ordLib, intLib, coreTypesLib, 
  bytesLib]

---------------------
-- Stack Manipulation

stackLib :: Library
stackLib = mkLib $ map mkStackCmd [
  ("nop", 0, id),
  ("id",  1, id),
  ("pop", 1, tail),
  ("dup", 1, \(p:ps)     -> p:p:ps),
  ("swp", 2, \(p:q:ps)   -> q:p:ps),
  ("rot", 3, \(c:b:a:ps) -> b:c:a:ps)
  ]

abcLib :: Library
abcLib = mkLib $ map mkStackCmd [
  ("abc", 3, id),
  ("acb", 3, \(b:a:ps)   -> a:b:ps),
  ("cba", 3, \(c:b:a:ps) -> a:b:c:ps),
  ("bac", 3, \(c:b:a:ps) -> c:a:b:ps),
  ("bca", 3, \(c:b:a:ps) -> b:c:a:ps),
  ("cab", 3, \(c:b:a:ps) -> b:a:c:ps)
  ]

runLib :: Library
runLib = [
  (fromString "run", Prm primRun),
  (fromString "dip", Prm primDip)
  ]

primRun (p:ps,env) = interpret p (ps,env)
primRun st         = underflow "run" st

primDip (p:q:ps,env) = do
  (ps',env') <- interpret p (ps,env)
  return (q:ps',env')
primDip st = underflow "dip" st

---------------
-- Higher Order

higherOrderLib :: Library
higherOrderLib = mkLib $ map mkStackCmd [
  ("quo", 1, \(p:ps) -> Quo p : ps),
  ("cat", 2, \(p:q:ps) -> Seq [q,p] : ps)
  ]

----------------------
-- Booleans and Choice

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
  | otherwise      = err (nm ++ ": expected: bool bool") st

primNot :: Program
primNot st@(stk,env)
  | null stk          = underflow "not" st
  | Bit b : ps <- stk = ok ((Bit $ not b):ps,env)
  | otherwise         = eRet 
  where
    eRet = err "not: expected: bool" st

primChoose :: Program
primChoose st@(stk,env)
  | length stk < 3 = underflow "choose" st
  | Bit True  : p : q : ps <- stk = ok (q:ps,env)
  | Bit False : p : q : ps <- stk = ok (p:ps,env)
  | otherwise                     = eRet
  where
    eRet = err "choose: expected: val val bool" st
    
---------------
-- Integer Math

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
  | otherwise      = err (nm ++ ": expected num num") st

primDivmod st@(Num b : Num a : ps, env)
  | b == 0    = err ("divmod: divide by zero") st
  | otherwise = ok (r:q:ps,env)
  where
    r = Num (a `mod` b)
    q = Num (a `div` b)
primDivmod st@(stk,env)
  | length stk < 2 = underflow "divmod" st
  | otherwise      = err ("divmod: expected num num") st

unaryMathPrim nm op st@(Num a : ps, env) = ok (Num (op a) : ps, env)
unaryMathPrim nm _  st = err (nm ++ ": expected num") st

------------------------
-- Ordering and Equality

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

-------------------
-- Basic Data Types

coreTypesLib :: Library
coreTypesLib = mkLib [
  ("unit",   Prm primUnit),
  ("pair",   Prm primPair),
  ("fst",    Prm primFst),
  ("snd",    Prm primSnd),
  ("unpari", Prm primUnpair),
  ("left",   Prm primLeft),
  ("right",  Prm primRight),
  ("alt",    Prm primAlt)
  ]

primUnit (ps,env) = ok (Vec [] : ps,env)

primPair (p:q:ps,env) = ok (Vec [q,p] : ps,env)
primPair st = underflow "pair" st

primFst (Vec [a,b] : ps,env) = ok (a : ps,env)
primFst st = err ("fst: expected: pair") st

primSnd (Vec [a,b] : ps,env) = ok (b : ps,env)
primSnd st = err ("snd: expected: pair") st

primUnpair (Vec [a,b] : ps,env) = ok (b : a : ps,env)
primUnpair st = err ("both: expected: pair") st

-- the extracting version of either can't be well-typed (?)

primLeft (p:ps,env) = ok (Vec [Bit False,p] : ps,env)
primLeft st = underflow "left" st

primRight (p:ps,env) = ok (Vec [Bit True,p] : ps,env)
primRight st = underflow "right" st

primAlt (Vec [Bit False,lv]:_:l:ps,env) = interpret l (lv:ps,env)
primAlt (Vec [Bit True, rv]:r:_:ps,env) = interpret r (rv:ps,env)
primAlt st = err ("alt: expected left-branch right-branch alt") st

----------------------
-- Bytes and Sequences

bytesLib :: Library
bytesLib = mkLib [
  ("chr",      Prm primChr),
  ("ord",      Prm primOrd),
  ("zeroes",   Prm primZeroes),
  ("len",      Prm primLen),
  ("strcat",   Prm primStrcat),
  ("substr",   Prm primSubstr),
  ("slice",    Prm primSlice)
  ]

primChr st@(Num n : ps,e)
  | n < 256   = ok (Str (singleByte n) : ps,e)
  | otherwise = err ("chr: int > 255") st
primChr st = err ("chr: expected: int") st

singleByte :: (Integral a, B.ByteArray b) => a -> b
singleByte = B.singleton . fromIntegral

primOrd st@(Str s : ps,e)
  | B.length s == 1 = ok (Num n : ps,e)
  | otherwise       = err ("ord: expected single byte") st
  where
    n = ord $ head $ toString s
primOrd st = err ("ord: expected: str") st

primZeroes st@(Num n : ps,e) = 
  ok (Str (B.pack $ replicate n $ fromIntegral 0) : ps,e)
primZeroes st = err ("zeroes: expected: int") st


primLen (Str s : ps,e) = ok ((Num $ B.length s) : ps,e)
primLen st = err ("len: expected: seq") st

primStrcat (Str  b : Str a : ps,env) = ok (Str (a<>b) : ps,env)
primStrcat st = err ("strcat: expected: seq seq") st

primSubstr
  (Num n : Num m : Str s : ps,e) = r where
    n' = if n == -1 then B.length s else n
    s' = B.take n' $ B.drop m s
    r  = ok (Str s':ps,e)
primSubstr st = err ("substr: expected: seq int int") st

primSlice 
  (Num n : Num m : Str s : ps,e) = r where
    n' = if n == -1 then B.length s else n
    s' = B.take (n'-m) $ B.drop m s
    r  = ok (Str s':ps,e)
primSlice st = err ("slice: expected: seq int int") st

--------
-- Notes
{-
-}
