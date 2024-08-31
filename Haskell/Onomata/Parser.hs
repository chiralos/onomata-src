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

module Onomata.Parser ( 
  parseExp, parseProg, printStack, parseStatic
  ) where

import Data.Char (chr)
import Data.List (intercalate)
import Data.Ratio
import Data.Semigroup ((<>))
import Numeric (readSigned,readDec,readHex)
import Text.Parsec
import Text.Parsec.Char

import Onomata.Types

parseExp,parseProg :: String -> String -> Either String Exp
parseExp  = parseX parseItem
parseProg = parseX parseSeq

parseX p fn = either (Left . show) Right . parse (parseTop p) fn

parseStatic :: String -> Exp
parseStatic s = case parseProg "" s of
  Right e -> e
  Left  _ -> undefined

type ParseExp    = Parsec [Char] () Exp
type ParseString = Parsec [Char] () String

parseNum, parseStr :: ParseExp
parseItem, parseSeq, parseSym, parseQuote :: ParseExp

parseTop p = do e <- p; eof; return e

parseSeq = spaces >> (Seq <$> (parseItem `sepEndBy` spaces))

parseItem = parseSym <|> parseQuote <|> parseStr <|> parseNum

parseSym = (Sym . fromString) <$> (box letter <> many parseSymChar)
  
parseSymChar = char '-' <|> char '/' <|> alphaNum

parseQuote = Quo <$> (between (char '(') (char ')') parseSeq)

parseStr = (Str . fromString) <$> 
  (between (char '"') (char '"') (many parseStrChar))

parseStrChar = (char '\\' >> parseEscChar) <|> noneOf ['"']

parseEscChar = parseHexEscChars <|> parseSingleEscChar

parseHexEscChars = 
  (chr . fst . head . readHex) <$> 
  (char 'x' >> box hexDigit <> box hexDigit)

parseSingleEscChar = choice [
  char '\\' >> return '\\',
  char '"'  >> return '"',
  char '0'  >> return '\0',
  char 'n'  >> return '\n',
  char 'r'  >> return '\r',
  char 't'  >> return '\t',
  char 'f'  >> return '\f',
  char 'v'  >> return '\v'
  ]


parseNum = try parseHexInt <|> parseDecInt

parseHexInt = 
  strToILit readHex <$>
  (optChar '-' <> (string "0x" >> many1 hexDigit))

parseDecInt =
  strToILit readDec <$>
  (optChar '-' <> many1 digit)

strToILit f = Num . fst . head . (readSigned f)

strChar c = box (char c)
optChar c = option [] (strChar c)
box       = ((:[]) <$>)

--

printExp (Str s)     = "\"" ++ (printEscChar =<< toString s) ++ "\""
printExp (Num n)     = show n 
printExp (Bit True)  = "true"
printExp (Bit False) = "false"
printExp (Tup v)     = printSeq "[" "]" v
printExp (Sym s)     = toString s
printExp (Seq ps)    = printSeq "(" ")" ps
printExp (Quo p)     = "(" ++ printExp p ++ ")"
printExp (Prm _)     = "(-prim-)"
  
printEscChar '\n' = "\\n"
printEscChar c    = [c]

printSeq o c exps = o ++ (intercalate " " $ map printExp exps) ++ c

printStack :: Stack -> String
printStack = (>>= ((++ "\n") . ("| " ++) . show))

instance Show Exp where
  show = printExp

--------
-- Notes
{-
  [1] Rational as numeric type:

  p = parseSignedInt <> option [] (
        strChar '.' <> parseInt  <> option [] (
          (strChar 'e' <|> strChar 'E') <> parseSignedInt))
-}
