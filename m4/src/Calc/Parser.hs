module Calc.Parser                    
(                    
pars                    
, Rep(..)                    
, Expr(..)                    
, Chast(..)                    
)
where 
import Pa
import HW2.T1
import HW2.T2
import HW2.T3
import HW2.T4 hiding (Expr(..), Val(..))
import HW2.T5
import HW2.T6


data Expr = Val String | Eval Expr Rep 
  deriving (Eq, Show)
data Rep = Cont Chast Rep | End Chast
  deriving (Eq, Show)
data Chast = Add Expr | Sub Expr | Mult Expr | Divi Expr | Fac String 
  deriving (Eq, Show)
  

pars = subexpr <* pEof
subexpr = expr
expr = do
  do
    v1 <- term
    v2 <- rep
    return (Calc.Parser.Eval v1 v2)
  <|> do
    v1 <- term
    return  v1

term = do
  do
    v1 <- factor
    v2 <- re
    return (Calc.Parser.Eval v1 v2)
  <|> do
    v1 <- factor
    v2 <- fa
    return (Calc.Parser.Eval v1 v2)
  <|> do
    v1 <- factor
    return  v1

factor = do
  do
    v1 <- tokens "("
    v2 <- expr
    v3 <- tokens ")"
    return  v2
  <|> do
    v1 <- num
    return  v1

num = do
  do
    v1 <- tokens "1"
    return (Calc.Parser.Val v1)
  <|> do
    v1 <- tokens "2"
    return (Calc.Parser.Val v1)
  <|> do
    v1 <- tokens "3"
    return (Calc.Parser.Val v1)
  <|> do
    v1 <- tokens "4"
    return (Calc.Parser.Val v1)

rep = do
  do
    v1 <- tokens "+"
    v2 <- term
    v3 <- rep
    return (Calc.Parser.Cont(Calc.Parser.Add v2) v3)
  <|> do
    v1 <- tokens "-"
    v2 <- term
    v3 <- rep
    return (Calc.Parser.Cont(Calc.Parser.Sub v2) v3)
  <|> do
    v1 <- tokens "+"
    v2 <- term
    return (Calc.Parser.End(Calc.Parser.Add v2))
  <|> do
    v1 <- tokens "-"
    v2 <- term
    return (Calc.Parser.End(Calc.Parser.Sub v2))

re = do
  do
    v1 <- tokens "*"
    v2 <- factor
    v3 <- re
    return (Calc.Parser.Cont(Calc.Parser.Mult v2) v3)
  <|> do
    v1 <- tokens "/"
    v2 <- factor
    v3 <- re
    return (Calc.Parser.Cont(Calc.Parser.Divi v2) v3)
  <|> do
    v1 <- tokens "*"
    v2 <- factor
    return (Calc.Parser.End(Calc.Parser.Mult v2))
  <|> do
    v1 <- tokens "/"
    v2 <- factor
    return (Calc.Parser.End(Calc.Parser.Divi v2))

fa = do
  do
    v1 <- tokens "^"
    v2 <- fa
    return (Calc.Parser.Cont(Calc.Parser.Fac v1) v2)
  <|> do
    v1 <- tokens "^"
    return (Calc.Parser.End(Calc.Parser.Fac v1))
