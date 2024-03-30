{-# LANGUAGE OverloadedStrings #-}
module AST where
import Data.Text (Text)

data Op0 
    = ReadByte 
    | PeekByte deriving (Show)

stringToOp0 :: Text -> Op0
stringToOp0 "read-byte" = ReadByte
stringToOp0 "peek-byte" = PeekByte
stringToOp0 "_"         = error "invalid op0"

data Op1 
    = Add1
    | Sub1
    | ZeroHuh
    | CharHuh
    | IntToChar
    | CharToInt
    | WriteByte
    | EOFObjectHuh
    | Neg
    | Abs
    | IntHuh
    | BoolHuh deriving (Show)

stringToOp1 :: Text -> Op1
stringToOp1 "add1"          = Add1
stringToOp1 "sub1"          = Sub1
stringToOp1 "zero?"         = ZeroHuh
stringToOp1 "char?"         = CharHuh
stringToOp1 "int->char"     = IntToChar
stringToOp1 "char->int"     = CharToInt
stringToOp1 "write-byte"    = WriteByte
stringToOp1 "eof-object?"   = EOFObjectHuh
stringToOp1 "-"             = Neg
stringToOp1 "abs"           = Abs
stringToOp1 "integer?"      = IntHuh
stringToOp1 "boolean?"      = BoolHuh
stringToOp1 _               = error "invalid op1"

data Op2 = Sub deriving (Show)

stringToOp2 :: Text -> Op2
stringToOp2 "-" = Sub
stringToOp2 _   = error "invalid op2"

data OpN = Add deriving (Show)

stringToOpN :: Text -> OpN
stringToOpN "+" = Add
stringToOpN _   = error "invalid opN"

type Id = Text

-- data Clause a = Clause a Expr deriving (Show)

-- data Datum = Integer Int | Boolean Bool | Character Char deriving (Show)
-- type CondClause = Clause Expr
-- type CaseClause = Clause [Datum]

data Expr
    = Integer Integer
    | Boolean Bool 
    | Character Char
    | Prim0 Op0
    | Prim1 Op1 Expr
    | Prim2 Op2 Expr Expr
    | PrimN OpN [Expr]
    | If Expr Expr Expr
    | Begin Expr Expr
    | Let [Id] [Expr] Expr
    | Var Id deriving (Show)
--    | LetStar [Id] [Expr] Expr
--    | Cond [CondClause] Expr
--    | Case Expr [CaseClause] Expr deriving (Show)