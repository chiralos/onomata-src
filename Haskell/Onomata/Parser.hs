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
  parseExp, parseProg, printStack
  ) where

import Data.Ratio
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Numeric (readSigned,readDec)
import Text.Parsec
import Text.Parsec.Char

import Onomata.Types

parseExp,parseProg :: String -> String -> Either String Exp
parseExp  = parseX parseItem
parseProg = parseX parseSeq

parseX p fn = either (Left . show) Right . parse (parseTop p) fn

type ParseExp    = Parsec [Char] () Exp
type ParseString = Parsec [Char] () String

parseNum, parseStr :: ParseExp
parseItem, parseSeq, parseSym, parseQuote :: ParseExp

parseTop p = do e <- p; eof; return e

parseSeq = spaces >> (Seq <$> (parseItem `sepEndBy` spaces))

parseItem = parseSym <|> parseQuote <|> parseStr <|> parseNum

parseSym = (Sym . fromString) <$> many1 parseSymChar
  
parseSymChar = char '/' <|> letter

parseQuote = Quo <$> (between (char '(') (char ')') parseSeq)

parseStr = parseBQStr <|> parseDQStr

-- HERE
parseDQStr = (Str . fromString) <$> 
  (between (char '"') (char '"') (many parseStrChar))

parseBQStr = do
  char '\''
  Sym s <- parseSym
  return (Str s)

parseStrChar = (char '\\' >> parseEscChar) <|> noneOf ['"']

parseEscChar = choice [
  char 'n'  >> return '\n',
  char '\\' >> return '\\',
  char '"'  >> return '"' ]

parseNum = strToILit <$> p where
  strToILit = Num . fst . head . (readSigned readDec)
  p         = parseSignedInt

parseInt, parseSignedInt :: ParseString
strChar, optChar :: Char -> ParseString

parseInt       = many1 digit
parseSignedInt = optChar '-' <> parseInt
strChar c      = ((:[]) <$> char c)
optChar c      = option [] (strChar c)

--

printExp (Str s)     = "\"" ++ (printEscChar =<< toString s) ++ "\""
printExp (Num n)     = show n
printExp (Bit True)  = "true"
printExp (Bit False) = "false"
printExp (Vec v)     = printSeq "[" "]" v
printExp (Ref _)     = "(-ref-)"
printExp (Sym s)     = toString s
printExp (Seq ps)    = printSeq "(" ")" ps
printExp (Quo p)     = "(" ++ printExp p ++ ")"
printExp (Prm _)     = "(-prim-)"
  
printEscChar '\n' = "\\n"
printEscChar c    = [c]

printSeq o c exps = o ++ (intercalate " " $ map printExp exps) ++ c

printStack :: Stack -> String
printStack = intercalate " " . map show . reverse

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
