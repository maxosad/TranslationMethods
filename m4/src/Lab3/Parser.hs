module Lab3.Parser                    
(                    
pars                    
, Rep(..)                    
, Expr(..)                    
, Chast(..)                    
, Text(..)                    
, Block(..)                    
, Str(..)                    
, Root(..)                    
, LogicExpr(..)                    
, IfExpr(..)                    
, ForExpr(..)                    
, WhileExpr(..)                    
)
where 
import Pa
import HW2.T1
import HW2.T2
import HW2.T3
import HW2.T4 hiding (Expr(..), Val(..), Prim(..))
import HW2.T5
import HW2.T6


data Text = Line Str 
          | Lines Str Text
          | LineIf IfExpr 
          | LinesIf IfExpr Text
          | LineWhile WhileExpr
          | LinesWhile WhileExpr Text
          | LineFor ForExpr
          | LinesFor ForExpr Text
  deriving (Eq)


data Block = Blck Text deriving (Eq)
 
data Str = Assign String Expr 
         | Declar String 
         | Set String Expr
         | Return Expr
  deriving (Eq)

data Root = RootBlock Block
  deriving (Eq)

data LogicExpr = BoolValue String 
               | Less Expr Expr
               | Greater Expr Expr
               | Greatereq Expr Expr
               | Lesseq Expr Expr
               | Eq Expr Expr
               | Noteq Expr Expr
               deriving (Eq)

data IfExpr = IfExpr LogicExpr Block
            | IfElExpr LogicExpr Block Block
  deriving (Eq)

data WhileExpr = WhileExpr LogicExpr Block
  deriving (Eq)


data ForExpr = ForExpr String Expr LogicExpr String Block
  deriving (Eq)


data Expr = Var String | Val String | Eval Expr Rep 
  deriving (Eq)
data Rep = Cont Chast Rep | End Chast
  deriving (Eq)
data Chast = Add Expr | Sub Expr | Mult Expr | Divi Expr 
  deriving (Eq)
  

pars = subexpr <* space <* pEof
subexpr = root
root = do
  do
    v1 <- tokens "int"
    v2 <- tokens "main"
    v3 <- tokens "("
    v4 <- tokens ")"
    v5 <- block
    return (Lab3.Parser.RootBlock v5)

block = do
  do
    v1 <- tokens "{"
    v2 <- text
    v3 <- tokens "}"
    return (Lab3.Parser.Blck v2)

text = do
  do
    v1 <- str
    v2 <- tokens ";"
    v3 <- text
    return (Lab3.Parser.Lines v1 v3)
  <|> do
    v1 <- str
    v2 <- tokens ";"
    return (Lab3.Parser.Line v1)
  <|> do
    v1 <- iff
    v2 <- text
    return (Lab3.Parser.LinesIf v1 v2)
  <|> do
    v1 <- iff
    return (Lab3.Parser.LineIf v1)
  <|> do
    v1 <- while
    v2 <- text
    return (Lab3.Parser.LinesWhile v1 v2)
  <|> do
    v1 <- while
    return (Lab3.Parser.LineWhile v1)

while = do
  do
    v1 <- tokens "while"
    v2 <- tokens "("
    v3 <- logic
    v4 <- tokens ")"
    v5 <- block
    return (Lab3.Parser.WhileExpr v3 v5)

str = do
  do
    v1 <- tokens "int"
    v2 <- tokens "a"
    v3 <- tokens "="
    v4 <- expr
    return (Lab3.Parser.Assign v2 v4)
  <|> do
    v1 <- tokens "int"
    v2 <- tokens "a"
    return (Lab3.Parser.Declar v2)
  <|> do
    v1 <- tokens "a"
    v2 <- tokens "="
    v3 <- expr
    return (Lab3.Parser.Set v1 v3)
  <|> do
    v1 <- tokens "return"
    v2 <- expr
    return (Lab3.Parser.Return v2)

iff = do
  do
    v1 <- tokens "if"
    v2 <- tokens "("
    v3 <- logic
    v4 <- tokens ")"
    v5 <- block
    v6 <- tokens "else"
    v7 <- block
    return (Lab3.Parser.IfElExpr v3 v5 v7)
  <|> do
    v1 <- tokens "if"
    v2 <- tokens "("
    v3 <- logic
    v4 <- tokens ")"
    v5 <- block
    return (Lab3.Parser.IfExpr v3 v5)

logic = do
  do
    v1 <- logicunary
    return  v1

logicunary = do
  do
    v1 <- tokens "True"
    return (Lab3.Parser.BoolValue v1)
  <|> do
    v1 <- tokens "False"
    return (Lab3.Parser.BoolValue v1)

expr = do
  do
    v1 <- term
    v2 <- rep
    return (Lab3.Parser.Eval v1 v2)
  <|> do
    v1 <- term
    return  v1

term = do
  do
    v1 <- factor
    v2 <- re
    return (Lab3.Parser.Eval v1 v2)
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
    v1 <- unary
    return  v1

unary = do
  do
    v1 <- tokens "1"
    return (Lab3.Parser.Val v1)
  <|> do
    v1 <- tokens "2"
    return (Lab3.Parser.Val v1)
  <|> do
    v1 <- tokens "a"
    return (Lab3.Parser.Var v1)

rep = do
  do
    v1 <- tokens "+"
    v2 <- term
    v3 <- rep
    return (Lab3.Parser.Cont(Lab3.Parser.Add v2) v3)
  <|> do
    v1 <- tokens "-"
    v2 <- term
    v3 <- rep
    return (Lab3.Parser.Cont(Lab3.Parser.Sub v2) v3)
  <|> do
    v1 <- tokens "+"
    v2 <- term
    return (Lab3.Parser.End(Lab3.Parser.Add v2))
  <|> do
    v1 <- tokens "-"
    v2 <- term
    return (Lab3.Parser.End(Lab3.Parser.Sub v2))

re = do
  do
    v1 <- tokens "*"
    v2 <- factor
    v3 <- re
    return (Lab3.Parser.Cont(Lab3.Parser.Mult v2) v3)
  <|> do
    v1 <- tokens "/"
    v2 <- factor
    v3 <- re
    return (Lab3.Parser.Cont(Lab3.Parser.Divi v2) v3)
  <|> do
    v1 <- tokens "*"
    v2 <- factor
    return (Lab3.Parser.End(Lab3.Parser.Mult v2))
  <|> do
    v1 <- tokens "/"
    v2 <- factor
    return (Lab3.Parser.End(Lab3.Parser.Divi v2))
