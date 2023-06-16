module Lab3.L                    
(                    
)
where 

import Lab3.Parser -- pars, datas
import HW2.T6
import HW2.T1 (Except(..))

{-
f :: String -> String
f s = show $ parse $ alexScanTokens s
-}
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



instance Show Str  where
  show (Assign var arithm)  = reformSpan "type" "int" ++ var ++ " = " ++ (show arithm)
  show (Declar var)         = reformSpan "type" "int" ++ var
  show (Set var arithm)     = var ++ " = " ++ (show arithm)
  show (Return arithm)     = reformSpan "blue" "return" ++ (show arithm)

instance Show LogicExpr where
    show (BoolValue x) =  x
    show (Less a b) = show a ++ " < " ++ show b
    show (Greater a b) = show a ++ " > " ++ show b
    show (Lesseq a b) = show a ++ " <= " ++ show b
    show (Greatereq a b) = show a ++ " >= " ++ show b
    show (Eq a b) = show a ++ " == " ++ show b
    show (Noteq a b) = show a ++ " != " ++ show b


instance Show IfExpr where
    show (IfExpr log b) = (reformSpan "blue" "if") ++ "(" ++ (show log) ++  ") " ++ (show b)
    show (IfElExpr log b1 b2) = (reformSpan "blue" "if") ++ "(" ++ (show log) ++ ") " ++ (show b1) ++ (reformSpan "blue" " else ") ++ (show b2) 


instance Show WhileExpr where
    show (WhileExpr log b) = (reformSpan "blue" "while") ++ "(" ++ (show log) ++ ") " ++ (show b)

instance Show ForExpr where
    show (ForExpr v arithm log v1 b) = (reformSpan "blue" "for") ++ "(" ++ reformSpan "type" "int" ++ v ++ " = " ++ (show arithm) ++ "; " ++ (show log) ++ "; " ++ v1 ++ "++" ++ ") " ++ (show b)


instance Show Expr  where
    show (Eval e r) = show e ++ show r
    show (Val x) = reformSpan "num" x
    show (Var v)   = v

    
instance Show Rep  where
    show (Cont a b) = show a ++ show b
    show (End a) = show a 


instance Show Chast  where
    show (Add a) = reformSpan "op" "+" ++ show a
    show (Sub a) = reformSpan "op" "-" ++ show a
    show (Mult a) = reformSpan "op" "*" ++ show a
    show (Divi a) = reformSpan "op" "/" ++ show a

{-

data Expr = Var String | Val String | Eval Expr Rep 
  deriving (Eq)
data Rep = Cont Chast Rep | End Chast
  deriving (Eq)
data Chast = Add Expr | Sub Expr | Mult Expr | Divi Expr 
  deriving (Eq)
  

-}

{-



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
    show (Add a b) = show a ++ reformSpan "op" "+" ++ show b
    show (Sub a b) = show a ++ reformSpan "op" "-" ++ show b
    show (Multi a b) = show a ++ reformSpan "op" "*" ++ show b
    show (Divi a b) = show a ++ reformSpan "op" "/" ++ show b
    show (Const x) = reformSpan "num" (show x)
    show (Var v)   = v


instance Show Str  where
  show (Assign var arithm)  = reformSpan "type" "int" ++ var ++ " = " ++ (show arithm)
  show (Declar var)         = reformSpan "type" "int" ++ var
  show (Set var arithm)     = var ++ " = " ++ (show arithm)
  show (Return arithm)     = reformSpan "blue" "return" ++ (show arithm)

instance Show LogicExpr where
    show (BoolValue x) = show x
    show (Less a b) = show a ++ " < " ++ show b
    show (Greater a b) = show a ++ " > " ++ show b
    show (Lesseq a b) = show a ++ " <= " ++ show b
    show (Greatereq a b) = show a ++ " >= " ++ show b
    show (Eq a b) = show a ++ " == " ++ show b
    show (Noteq a b) = show a ++ " != " ++ show b


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
  -}

    
fromSucc (Success s) = s 



e0 = fromSucc $ runP pars "int main() {int a; a = 1; int a; a = 1; int a = 2;}"
e1 = fromSucc $ runP pars "int main() {int a;}"
e2 = fromSucc $ runP pars "int main() {a=1;}"
e3 = fromSucc $ runP pars "int main() {inta=2;}"
e4 = fromSucc $ runP pars "int main() {if (True) {int a = 1;}}"


 

main :: IO ()
main = do
    style <- readFile "src/Lab3/style.txt"
    writeFile "src/Lab3/index.html" style
    code <- readFile "src/Lab3/input2.txt"
    appendFile "src/Lab3/index.html" (show ( fromSucc ( runP pars code)))
  

