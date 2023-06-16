{-# LANGUAGE InstanceSigs #-}

module Parser.Generator
  ( 
  )
where

import Le
import Pa
import Data.Char -- toLower
rulenum = 0

data Ty = Neterm | TerminalInt Int | TerminalString String

{-

f :: String -> String
f s = show $ parse $ alexScanTokens s


main :: IO ()
main = do
    s <- readFile "grammer2.txt"
    putStrLn $ f s
-}

toLowerString :: String -> String 
toLowerString str = map toLower str


main :: IO ()
main = do
    writeFile "src/Calc/Parser.hs" headercalc----------------
    s <- readFile "src/Calc/grammer.txt"
    d <- readFile "src/Calc/data.txt"
    let gram = Pa.parse $ alexScanTokens s
    let newmap = map genrule gram
    let str = foldl1  (\a b -> a ++ "\n" ++ b) newmap
    appendFile "src/Calc/Parser.hs" d
    appendFile "src/Calc/Parser.hs" cha
    appendFile "src/Calc/Parser.hs" str 
    writeFile "src/Lab2/Parser.hs" headerlab2------------
    s2 <- readFile "src/Lab2/grammer.txt"
    d2 <- readFile "src/Lab2/data.txt"
    let gram2 = Pa.parse $ alexScanTokens s2
    let newmap2 = map genrule2 gram2
    let str2 = foldl1  (\a b -> a ++ "\n" ++ b) newmap2
    appendFile "src/Lab2/Parser.hs" d2
    appendFile "src/Lab2/Parser.hs" cha
    appendFile "src/Lab2/Parser.hs" str2 
    writeFile "src/Lab3/Parser.hs" headerlab3----------------
    s3 <- readFile "src/Lab3/grammer.txt"
    d3 <- readFile "src/Lab3/data.txt"
    let gram3 = Pa.parse $ alexScanTokens s3
    let newmap3 = map genrule3 gram3
    let str3 = foldl1  (\a b -> a ++ "\n" ++ b) newmap3
    appendFile "src/Lab3/Parser.hs" d3
    appendFile "src/Lab3/Parser.hs" cha3
    appendFile "src/Lab3/Parser.hs" str3
--Lab3 headerlab3
cha :: String 
cha = "\npars = subexpr <* pEof\nsubexpr = expr\n"
cha3 = "\npars = subexpr <* space <* pEof\nsubexpr = root\n"


genrule3 :: Rule -> String
genrule3 (Rule name arr ) = toLowerString name ++ " = do\n  do\n" ++ foldl1  (\a b -> a ++ "\n  <|> do\n" ++ b ) (map uno3 arr) ++ "\n"-- разбираем arr

uno3 :: Tmp -> String
uno3 (Uno ins constr) = foldl1 (\a b -> a ++ "\n" ++ b ) (prins 1 ins) ++ "\n    return " ++ obr3 constr

obr3 :: Constructor -> String 
obr3 (Inv a) = " v" ++ show a
obr3 (Constrname name args) = "(Lab3.Parser." ++name ++ (foldl (\a b-> a ++ obr3 b ) "" args) ++ ")"


-------------
genrule2 :: Rule -> String
genrule2 (Rule name arr ) = toLowerString name ++ " = do\n  do\n" ++ foldl1  (\a b -> a ++ "\n  <|> do\n" ++ b ) (map uno2 arr) ++ "\n"-- разбираем arr

uno2 :: Tmp -> String
uno2 (Uno ins constr) = foldl1 (\a b -> a ++ "\n" ++ b ) (prins 1 ins) ++ "\n    return " ++ obr2 constr

obr2 :: Constructor -> String 
obr2 (Inv a) = " v" ++ show a
obr2 (Constrname name args) = "(Lab2.Parser." ++name ++ (foldl (\a b-> a ++ obr2 b ) "" args) ++ ")"

--------

genrule :: Rule -> String
genrule (Rule name arr ) = toLowerString name ++ " = do\n  do\n" ++ foldl1  (\a b -> a ++ "\n  <|> do\n" ++ b ) (map uno arr) ++ "\n"-- разбираем arr
   -- arr == [(Uno)]

-- рекурсивная функция с поддержкой номера будет вызываться 
-- для каждого Uno
-- data Tmp = Uno [In] Constructor
uno :: Tmp -> String
uno (Uno ins constr) = foldl1 (\a b -> a ++ "\n" ++ b ) (prins 1 ins) ++ "\n    return " ++ obr constr
  
obr :: Constructor -> String 
obr (Inv a) = " v" ++ show a
obr (Constrname name args) = "(Calc.Parser." ++name ++ (foldl (\a b-> a ++ obr b ) "" args) ++ ")"
 --"(Parser."++ n
--obr args = foldl (\a b -> a ++ " v" ++ (show b)   ) "" args

-- prins :: [Stroka "Factor",Plus,Stroka "Expr"] -> [v1 <- factor]
prins :: Int -> [In] -> [String]
prins n (i:is) = ("    v" ++ (show n) ++ " <- " ++ (decompose i)) : prins (n+1) is
prins n [] = []

--data In = Stroka String | Plus | Minus | Mul | Division
decompose :: In -> String 
decompose (Stroka s) = toLowerString s 
decompose Plus = "tokens \"+\"" 
decompose Minus = "tokens \"-\""
decompose Mul = "tokens \"*\""
decompose Division = "tokens \"/\""
decompose (TokStr s)= "tokens \""++ s ++ "\""
decompose (TokInt n)= "tokens \""++ (show n) ++ "\""





e = "Expr = Factor + Expr {Add $1 $3} | Factor { $1 } ; Factor = #qwerty# { $1}; "
e1 = "Expr = Term {Add $1 $3} ; Expr = Term {Add $1 $3} ;"

tes =Pa.parse $ alexScanTokens e
test = alexScanTokens e




  
headercalc ="module Calc.Parser                    \n\
        \(                    \n\
        \pars                    \n\
        \, Rep(..)                    \n\
        \, Expr(..)                    \n\
        \, Chast(..)                    \n\
        \)\n\
        \where \n\
        \import Pa\n\
        \import HW2.T1\n\
        \import HW2.T2\n\
        \import HW2.T3\n\
        \import HW2.T4 hiding (Expr(..), Val(..))\n\
        \import HW2.T5\n\
        \import HW2.T6\n\n\n"



headerlab2 ="module Lab2.Parser                    \n\
        \(                    \n\
        \pars                    \n\
        \, Primintive (..)                    \n\
        \, Expr  (..)                    \n\
        \)\n\
        \where \n\
        \import Pa\n\
        \import HW2.T1\n\
        \import HW2.T2\n\
        \import HW2.T3\n\
        \import HW2.T4 hiding (Expr(..), Val(..), Prim(..))\n\
        \import HW2.T5\n\
        \import HW2.T6\n\n\n"

headerlab3 ="module Lab3.Parser                    \n\
        \(                    \n\
        \pars                    \n\
        \, Rep(..)                    \n\
        \, Expr(..)                    \n\
        \, Chast(..)                    \n\
        \, Text(..)                    \n\
        \, Block(..)                    \n\
        \, Str(..)                    \n\
        \, Root(..)                    \n\
        \, LogicExpr(..)                    \n\
        \, IfExpr(..)                    \n\
        \, ForExpr(..)                    \n\
        \, WhileExpr(..)                    \n\
        \)\n\
        \where \n\
        \import Pa\n\
        \import HW2.T1\n\
        \import HW2.T2\n\
        \import HW2.T3\n\
        \import HW2.T4 hiding (Expr(..), Val(..), Prim(..))\n\
        \import HW2.T5\n\
        \import HW2.T6\n\n\n"
        
        
glav = "expr = subexpr `chainr1` pow `chainl1` mul `chainl1` add \n\
  \subexpr = token \"(\" *> expr <* token \")\" <|> integer \n\n\n\n"

