{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.SCargot
import Data.SCargot.Repr.Basic
import Data.Text (Text, pack)
import Numeric (readHex)
import Text.Parsec (anyChar, char, digit, many1, manyTill, newline, satisfy, string)
import Text.Parsec.Text (Parser)

-- Our operators are going to represent addition, subtraction, or
-- multiplication
data Op = Add | Sub | Mul | Div deriving (Eq, Show)

-- The atoms of our language are either one of the aforementioned
-- operators, or positive integers
data Atom = AOp Op | ANum Int deriving (Eq, Show)

-- Once parsed, our language will consist of the applications of
-- binary operators with literal integers at the leaves
data Expr = Op Op Expr Expr | Num Int deriving (Eq, Show)

-- Conversions to and from our Expr type
toExpr :: SExpr Atom -> Either String Expr
toExpr (A (AOp op) ::: l ::: r ::: Nil) = Op op <$> toExpr l <*> toExpr r
toExpr (A (ANum n)) = pure (Num n)
toExpr sexpr = Left ("Unable to parse expression: " ++ show sexpr)

fromExpr :: Expr -> SExpr Atom
fromExpr (Op op l r) = A (AOp op) ::: fromExpr l ::: fromExpr r ::: Nil
fromExpr (Num n)     = A (ANum n) ::: Nil

-- Parser and serializer for our Atom type
pAtom :: Parser Atom
pAtom = ((ANum . read) <$> many1 digit)
     <|> (char '+' *> pure (AOp Add))
     <|> (char '-' *> pure (AOp Sub))
     <|> (char '*' *> pure (AOp Mul))
     <|> (char '/' *> pure (AOp Div))

sAtom :: Atom -> Text
sAtom (AOp Add) = "+"
sAtom (AOp Sub) = "-"
sAtom (AOp Mul) = "*"
sAtom (ANum n)  = pack (show n)

-- Our custom reader macro: grab the parse stream and read a
-- hexadecimal number from it:
hexReader :: Reader Atom
hexReader _ = (A . ANum . rd) <$> many1 (satisfy isHexDigit)
  where isHexDigit c = isDigit c || c `elem` hexChars
        rd = fst . head . readHex
        hexChars :: String
        hexChars = "AaBbCcDdEeFf"

-- Our final s-expression parser and printer:
myLangParser :: SExprParser Atom Expr
myLangParser
  = addReader '#' hexReader     -- add hex reader
  $ setCarrier toExpr           -- convert final repr to Expr
  $ mkParser pAtom              -- create spec with Atom type

mkLangPrinter :: SExprPrinter Atom Expr
mkLangPrinter
  = setFromCarrier fromExpr
  $ setIndentStrategy (const Align)
  $ basicPrint sAtom