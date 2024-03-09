{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.SCargot
    ( addReader, mkParser, setCarrier, Reader, SExprParser )
import Data.SCargot.Repr.Basic
import Data.Text (Text, pack)
import Numeric (readDec)
import Text.Parsec (anyChar, char, digit, many1, manyTill, newline, satisfy, string)
import Text.Parsec.Text (Parser)
import Data.Functor (($>))

data Op = Add | Sub | Mul | Div deriving (Eq, Show)

data Atom = AOp Op | ANum Int deriving (Eq, Show)

data Expr = Op Op Expr Expr | Num Int deriving (Eq, Show)

toExpr :: SExpr Atom -> Either String Expr
toExpr (A (AOp op) ::: l ::: r ::: Nil) = Op op <$> toExpr l <*> toExpr r
toExpr (A (ANum n)) = pure (Num n)
toExpr sexpr = Left ("Unable to parse expression: " ++ show sexpr)

pAtom :: Parser Atom
pAtom = (ANum . read <$> many1 digit)
     <|> (char '+' $> AOp Add)
     <|> (char '-' $> AOp Sub)
     <|> (char '*' $> AOp Mul)
     <|> (char '/' $> AOp Div)

sAtom :: Atom -> Text
sAtom (AOp Add) = "+"
sAtom (AOp Sub) = "-"
sAtom (AOp Mul) = "*"
sAtom (ANum n)  = pack (show n)

decReader :: Reader Atom
decReader _ = A . ANum . rd <$> many1 (satisfy isDigit)
  where rd = fst . head . readDec

parser :: SExprParser Atom Expr
parser = addReader '#' decReader $ setCarrier toExpr $ mkParser pAtom              