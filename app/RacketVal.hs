module RacketVal (
  RacketVal(..),
) where

import qualified Data.Text as T

data RacketVal
  = Atom T.Text
  | List [RacketVal]
  | Number Integer
  | Bool Bool
  | Char Char
  deriving (Eq, Show)