{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Parse where
import AST
import RacketVal
import SExpr
import Data.Text (Text)

parse :: RacketVal -> Expr
parse (Number i)                                        = Integer i
parse (Bool b)                                          = Boolean b
parse (Char c)                                          = Character c
parse (Atom s)                                          = Var s
parse (List [Atom "not", e])                            = If (parse e) (Boolean False) (Boolean True)
parse (List [Atom "begin", e1, e2])                     = Begin (parse e1) (parse e2)
parse (List [Atom "let", List bs, e])                   = parseLet bs e
parse (List [Atom "let*", List bs, e])                  = parseLetStar bs e
parse (List [Atom "if", e1, e2, e3])                    = If (parse e1) (parse e2) (parse e3)
parse (List ((Atom "cond"):cs))                         = parseCond cs
parse (List ((Atom "case"):ev:cs))                      = parseCase ev cs
parse (List [Atom op0])         | op0 `elem` validOp0   = Prim0 (stringToOp0 op0)
parse (List [Atom op1, e])      | op1 `elem` validOp1   = Prim1 (stringToOp1 op1) (parse e)
parse (List [Atom op2, e1, e2]) | op2 `elem` validOp2   = Prim2 (stringToOp2 op2) (parse e1) (parse e2)
parse (List (Atom opN:xs))      | opN `elem` validOpN   = PrimN (stringToOpN opN) (map parse xs)
parse _                                                 = error "parse error"

parseLet :: [RacketVal] -> RacketVal -> Expr
parseLet [] e                           = LetStd [] [] (parse e)
parseLet ((List [Atom id, e1]):bs) e    = let (LetStd xs es e') = parseLet bs e in LetStd (id:xs) ((parse e1):es) e'
parseLet _ _                            = error "let parse error"

parseLetStar :: [RacketVal] -> RacketVal -> Expr
parseLetStar [] e = LetStar [] [] (parse e)
parseLetStar ((List [Atom id, e1]):bs) e = let (LetStar xs es e') = parseLetStar bs e in LetStar (id:xs) ((parse e1):es) e'
parseLetStar _ _                        = error "let* parse error"

parseCond :: [RacketVal] -> Expr
parseCond [List [Atom "else", e]] = Cond [] (parse e)
parseCond ((List [e1, e2]):cs)    = let Cond cs' els = parseCond cs in Cond (Clause (parse e1) (parse e2):cs') els
parseCond _                       = error "cond parse error"

parseCase :: RacketVal -> [RacketVal] -> Expr
parseCase ev [List [Atom "else", e]] = Case (parse ev) [] (parse e)
parseCase ev ((List [List ds, e]):cs)     = let Case ev' cs' el = parseCase ev cs in Case ev' (Clause (parseDatums ds) (parse e):cs') el
parseCase _ _                        = error "case parse error"

parseDatums :: [RacketVal] -> [Expr]
parseDatums [] =  []
parseDatums ((Number n):xs) = Integer n:parseDatums xs
parseDatums ((Bool b):xs)   = Boolean b:parseDatums xs
parseDatums ((Char c):xs)   = Character c:parseDatums xs
parseDatums _               = error "datums parse error"
   
parseSExpr :: Text -> Expr
parseSExpr s = case readExpr s of
    Left _ -> error "invalid s-expression"
    Right e -> parse e