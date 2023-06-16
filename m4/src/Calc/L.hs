module Calc.L                    
(                    
)
where 

import Calc.Parser (pars, Rep(..), Expr(..), Chast(..) )
import HW2.T6
import HW2.T1 (Except(..))


{-
data Expr = Eval Term Rep | Zn Term
  deriving (Eq, Show)

data Rep = Cont Op Rep | Valu Op
  deriving (Eq, Show)

data Op = Add Term | Sub Term 
  deriving (Eq, Show)

data Term = Val String 
  deriving (Eq, Show)
  
  Expr = Term Rep {Eval $1 $2} | Term {Zn $1} ;
Rep =   - Term Rep {Cont (Sub $2) $3} | + Term Rep {Cont (Add $2) $3} | + Term {Valu (Add $2)} | - Term {Valu (Sub $2)} ;
Term = Num { $1};
Num = #0# {Val $1} | #1# {Val $1} |  #2# {Val $1};
  
data Expr = Val String | Eval Expr Rep 
  deriving (Eq, Show)
data Rep = Cont Chast Rep | End Chast
  deriving (Eq, Show)
data Chast = Add Expr | Sub Expr | Mult Expr | Divi Expr 
  deriving (Eq, Show)
  

  -}
mainexpr = pars
eval :: Expr -> String
eval (Val s) = s
eval (Eval e repit) = fol (eval e) repit
  
  

  

fac 0 = 1
fac n = n * fac (n - 1)

app :: String -> Chast -> String
app v chast = let 
  a = read v :: Int
  in 
    case chast of 
      (Add ev ) -> show (a + (read (eval ev)::Int))
      (Sub ev ) -> show (a - (read (eval ev)::Int))
      (Mult ev ) -> show (a * (read (eval ev)::Int))
      (Divi ev ) -> show (a + (read (eval ev)::Int))
      (Fac ev) -> show (fac a) 
  
fol :: String -> Rep -> String 
fol v (Cont ct repit) = fol (app v ct) repit
fol v (End ct) = app v ct 

fromSucc (Success s) = s 





e  =eval $ fromSucc $ runP mainexpr "1^+2^+3^"
e1 =eval $ fromSucc $ runP mainexpr "1!+2!+3!"
e2 =eval $ fromSucc $ runP mainexpr "2*2+(2+2)*2"
e3 =eval $ fromSucc $ runP mainexpr "2*2+2+2*2"
e31 =eval $ fromSucc $ runP mainexpr "(2*2+2+2*2)!"
e4 =eval $ fromSucc $ runP mainexpr "3!"
e41 =eval $ fromSucc $ runP mainexpr "4!"
e5 =eval $ fromSucc $ runP mainexpr "3!!"

main = do 
  putStrLn e
 



{-
Success (Eval 
          (
            Eval (Val "2") 
            (End (Mult (Val "2")))
          )
          (
            End (
              Add (
                Eval (
                  Eval (Val "2") 
                  (End (Add (Val "2")))
                ) 
                (End (Divi (Val "2")))
              )
            )
          )
        )
-}