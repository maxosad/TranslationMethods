module Main where

import Lexer
import Parser



f :: String -> String
f s = show $ parse $ alexScanTokens s

reformSpan :: String -> String -> String
reformSpan cls sym = " <span class = \"" ++ cls ++ "\">" ++ sym ++ "</span> "

reformDiv :: String -> String -> String
reformDiv cls sym = " <div class = \"" ++ cls ++ "\">" ++ sym ++ "</div> "


instance Show Root  where
  show (RootBlock y ) = reformSpan "type" "int" ++ "main() " ++ (show y)

instance Show Text  where
  show (Line str) = "<div>" ++ (show str) ++ " ;</div>"
  show (Lines str ost) = "<div>" ++ (show str) ++ " ;</div>" ++ (show ost)
  show (LineIf iff) = "<div>" ++ (show iff) ++ " </div>"
  show (LinesIf iff ost) = "<div>" ++ (show iff) ++ " </div>" ++ (show ost)
  show (LineWhile wh) = "<div>" ++ (show wh) ++ " </div>"
  show (LinesWhile wh ost) = "<div>" ++ (show wh) ++ " </div>" ++ (show ost)
  show (LineFor fr) = "<div>" ++ (show fr) ++ " </div>"
  show (LinesFor fr ost) = "<div>" ++ (show fr) ++ " </div>" ++ (show ost)
  
instance Show Block  where
  show (Blck str) = "{\n" ++ reformDiv "block" (show str) ++ "\n}\n"


instance Show ArithmExpr  where
    show (a :+: b) = show a ++ reformSpan "op" "+" ++ show b
    show (a :-: b) = show a ++ reformSpan "op" "-" ++ show b
    show (a :*: b) = show a ++ reformSpan "op" "*" ++ show b
    show (a :/: b) = show a ++ reformSpan "op" "/" ++ show b
    show (Const x) = reformSpan "num" (show x)
    show (Var v)   = v


instance Show Str  where
  show (Assign var arithm)  = reformSpan "type" "int" ++ var ++ " = " ++ (show arithm)
  show (Declar var)         = reformSpan "type" "int" ++ var
  show (Set var arithm)     = var ++ " = " ++ (show arithm)
  show (Return arithm)     = reformSpan "blue" "return" ++ (show arithm)

instance Show LogicExpr where
    show (BoolValue x) = show x
    show (a :<: b) = show a ++ " < " ++ show b
    show (a :>: b) = show a ++ " > " ++ show b
    show (a :<=: b) = show a ++ " <= " ++ show b
    show (a :>=: b) = show a ++ " >= " ++ show b
    show (a :==: b) = show a ++ " == " ++ show b
    show (a :!=: b) = show a ++ " != " ++ show b


instance Show IfExpr where
    show (IfExpr log b) = (reformSpan "blue" "if") ++ "(" ++ (show log) ++  ") " ++ (show b)
    show (IfElExpr log b1 b2) = (reformSpan "blue" "if") ++ "(" ++ (show log) ++ ") " ++ (show b1) ++ (reformSpan "blue" " else ") ++ (show b2) 


instance Show WhileExpr where
    show (WhileExpr log b) = (reformSpan "blue" "while") ++ "(" ++ (show log) ++ ") " ++ (show b)

instance Show ForExpr where
    show (ForExpr v arithm log v1 b) = (reformSpan "blue" "for") ++ "(" ++ reformSpan "type" "int" ++ v ++ " = " ++ (show arithm) ++ "; " ++ (show log) ++ "; " ++ v1 ++ "++" ++ ") " ++ (show b)

    

main :: IO ()
main = do
    s <- readFile "input.txt"
    putStrLn $ f s