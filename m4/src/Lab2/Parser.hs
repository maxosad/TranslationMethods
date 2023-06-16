module Lab2.Parser                    
(                    
pars                    
, Primintive (..)                    
, Expr  (..)                    
)
where 
import Pa
import HW2.T1
import HW2.T2
import HW2.T3
import HW2.T4 hiding (Expr(..), Val(..), Prim(..))
import HW2.T5
import HW2.T6


data Primintive a = Choise a a | Klin a | Add a a
  deriving (Eq, Show)
  

data Expr = Val String | Op (Primintive Expr) 
  deriving (Eq, Show)
pars = subexpr <* pEof
subexpr = expr
term = do
  do
    v1 <- tokens "a"
    v2 <- tokens "*"
    return (Lab2.Parser.Op(Lab2.Parser.Klin(Lab2.Parser.Val v1)))
  <|> do
    v1 <- tokens "a"
    return (Lab2.Parser.Val v1)
  <|> do
    v1 <- tokens "("
    v2 <- expr
    v3 <- tokens ")"
    v4 <- tokens "*"
    return (Lab2.Parser.Op(Lab2.Parser.Klin v2))
  <|> do
    v1 <- tokens "("
    v2 <- expr
    v3 <- tokens ")"
    return  v2

expr = do
  do
    v1 <- term
    v2 <- tokens "|"
    v3 <- expr
    return (Lab2.Parser.Op(Lab2.Parser.Choise v1 v3))
  <|> do
    v1 <- term
    v2 <- expr
    return (Lab2.Parser.Op(Lab2.Parser.Add v1 v2))
  <|> do
    v1 <- term
    return  v1
