{-# LANGUAGE OverloadedStrings #-}
module Parse where
import AST
import RacketVal
import SExpr
import Data.Text (Text)

parse :: RacketVal -> Expr
parse (Number i)                        = Integer i
parse (Bool b)                          = Boolean b
parse (Char c)                          = Character c
parse (Atom s)                          = Var s
parse (List [Atom "not", e])            = If (parse e) (Boolean False) (Boolean True)
parse (List [Atom "begin", e1, e2])     = Begin (parse e1) (parse e2)
parse (List [Atom "let", List bs, e])   = parseLet bs e
parse (List [Atom "if", e1, e2, e3])    = If (parse e1) (parse e2) (parse e3)
parse (List [Atom op0])                 = Prim0 (stringToOp0 op0)
parse (List [Atom op1, e])              = Prim1 (stringToOp1 op1) (parse e)
parse (List [Atom op2, e1, e2])         = Prim2 (stringToOp2 op2) (parse e1) (parse e2)
parse (List (Atom opN:xs))              = PrimN (stringToOpN opN) (map parse xs) -- to do: fix opN so (+) is a valid expression
parse _                                 = error "parse error"

parseLet :: [RacketVal] -> RacketVal -> Expr
parseLet [] e                           = Let [] [] (parse e)
parseLet ((List [Atom id, e1]):bs) e    = let (Let xs es e') = parseLet bs e in Let (id:xs) (parse e1:es) e'
parseLet _ _                            = error "let parse error"

parseSExpr :: Text -> Expr
parseSExpr s = case readExpr s of
    Left _ -> error "invalid s-expression"
    Right e -> parse e