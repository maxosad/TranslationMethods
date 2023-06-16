{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HW2.T6 
  ( Parser(..)
  , chainl1
  , chainr1
  , token
  , tokens
  , runP
  , pEof
  , (<|>)
  , space
  ) where

import HW2.T1 hiding(Pair(..))
import HW2.T2
import HW2.T5
import Numeric.Natural (Natural)
import Control.Applicative 
import Control.Monad
import Data.Char


{-
opExpr leftAssoc op next = do
  start <- next
  rest <- many $ do
    o <- op
    r <- next
    return (o, r)
  let folder = if leftAssoc then foldLeft else foldr
  return $ folder (\(op, r) l -> mkOpCall op l r) start next


mkOpCall
-}

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest
  where rest a = ((op <*> pure a <*> p) >>= rest)  <|> return a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = p >>= rest
  where rest a =  (op <*> pure a <*> (p >>= rest)) <|> return a

data ParseError = ErrorAtPos Natural deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P es) str = per ((runES es) (0,str))

per :: Except ParseError (Annotated (Natural, String) a) -> Except ParseError a
per (Error e) = (Error e)
per (Success (a:#e)) = (Success a)


pChar :: Parser Char
pChar = P $ ES (\(pos, s) -> case s of
  []     -> Error (ErrorAtPos pos)
  (c:cs) -> Success (c :# (pos + 1, cs)))

pCharSat :: Char -> Parser Char
pCharSat x = P $ ES (\(pos, s) -> case s of
  []     -> Error (ErrorAtPos pos)
  (c:cs) -> if (c == x)
    then Success (c :# (pos + 1, cs))
    else Error (ErrorAtPos pos))

proverca = do
  do  
    token 'a'
    return "a"
  <|> do
    token 'b'
    return "b"
    
pp1 = "a"
pp2 = "b"
testprov = runP mepr pp2

mepr1 = proverca <* pEof


  
parseError :: Parser a  
parseError = P $ ES (\(pos, s) -> Error (ErrorAtPos pos))

parserSuccess :: ExceptState ParseError (Natural, String) Double
parserSuccess = ES {runES = \s -> wrapExcept (1.0:#s)}
     
  
instance Alternative Parser where
  empty = parseError
  (P e1) <|> (P e2) = P $ ES (\(pos,s) -> 
    let s1 = runES e1 (pos,s) in
      case s1 of
        (Error e) -> runES e2 (pos,s)
        _ -> s1
    ) 


instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES (\(pos, s) -> case s of
  [] -> Success (() :# (pos + 1, ""))
  _  -> (Error (ErrorAtPos pos)))


satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P $  ES (\(pos, s) -> case s of
  (c:cs) -> (if f c 
    then Success (c :# (pos + 1, cs))
    else Error (ErrorAtPos pos))
  []     -> Error (ErrorAtPos pos)
  )
  
  
tokens :: String -> Parser String 
tokens s = space *> (word s)

word :: String -> Parser String
word (b:bs) = do
  v1 <- pCharSat b
  v2 <- word bs
  return (v1 : v2)
word [] = return ""
  
  
testword = "qwerty"

testprovword = runP mepr2 testword

mepr2 = tokens "qwerty" <* pEof

char :: Char -> Parser Char
char c = satisfy (== c)

space :: Parser String -- пробелов может и не быть 
space = many (char ' ')

token :: Char -> Parser Char
token c = space *> char c






data Prim a =
    Choise a a      -- (|)
  | Klin a          -- (*)
  | Plus a          -- (+)
  | Add a a          -- (:)
  deriving Show

data Expr = Val Char | Op (Prim Expr) deriving Show

pChoise :: Parser (Expr -> Expr -> Expr)
pChoise = token '|' *> pure (choise) 

choise :: Expr -> Expr -> Expr
choise e1 e2 = (Op (Choise e1 e2))


star :: Expr -> Expr
star c = (Op (Klin c))

pls :: Expr -> Expr
pls c = (Op (Plus c))

conc :: Expr -> Expr -> Expr
conc e1 e2 = (Op (Add e1 e2))

pConcat :: Parser (Expr -> Expr -> Expr)
pConcat = pure (conc)

lasoc :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
p `lasoc` op = p >>= rest
  where rest a = ((op <*> pure a <*> p) >>= rest)  <|> return a

d0 = star (Val 'e')
d1 = choise d0 d0



pE :: Parser Expr
pE = do 
  c <- satisfy isLetter
  pure (Val c)


pE1 :: Parser Expr
pE1 = do 
  c <- satisfy isLetter
  pure (Val c)
  
ex :: Parser Expr
ex = term *> token '+' *> ex <|>  term *> token '-' *> ex <|> term

term = factor *> token '*' *> term <|> factor  *> token '/' *> term <|> factor;


factor = token '(' *> ex <* token ')' <|> pE





{-
mepr :: Parser Expr
mepr = ex <* pEof
-}
--M
mepr :: Parser Expr
mepr = expr <* pEof

--E
expr :: Parser Expr
expr = subexpr `lasoc` pChoise `lasoc` pConcat
--S
subexpr :: Parser Expr
subexpr = do 
  exp <- token '(' *> expr <* token ')' <|> pE -- E1
  k <- optional ((char '*') <|> (char '+'))
  case k of -- K
    Nothing -> pure exp
    (Just '*') -> pure (star exp)
    (Just '+') -> pure (pls exp)






g0 = runP mepr "(b)+(b)/b" -- In Example E ->  S: S


e0 = runP mepr "((abc*b|a)*ab(aa|b*)b)*" -- In Example E ->  S: S
e1 = runP mepr "((c*b|a)*b(a|b*)b)*"

s1 = runP mepr "a*" -- S -> E1 K
s2 = runP mepr "(a*)"
s3 = runP mepr "(a)*"
s4 = runP mepr "(a)**"

c0 = runP mepr "a|b" -- E -> S | S
c1 = runP mepr "a*|*b*"
c2 = runP mepr "a*|b*"
c3 = runP mepr "a*|"

a0 = runP mepr "abcde" -- E ->  S: S

p1 = runP mepr "ab(xa)*+a"
p2 = runP mepr "(a+a+)+a"
p3 = runP mepr "a+|"
p4 = runP mepr "a+|b*"
p5 = runP mepr "a+a+w+"

-- K -> * // + // ''

main = do 
  putStr ( "------START----------\n")
  putStr ("g0 :" ++ show g0 ++ "\n")
  putStr ("e0 :" ++ show e0 ++ "\n")
  putStr ("e1 :" ++ show e1 ++ "\n")
  putStr ( "------SECTION S -> E1 K \n----------\n")
  putStr ("s1 :" ++ show s1 ++ "\n")
  putStr ("s2 :" ++ show s2 ++ "\n")
  putStr ("s3 :" ++ show s3 ++ "\n")
  putStr ("s4 :" ++ show s4 ++ "\n")
  putStr ( "------SECTION E -> S | S----------\n")
  putStr ("c0 :" ++ show c0 ++ "\n")
  putStr ("c1 :" ++ show c1 ++ "\n")
  putStr ("c2 :" ++ show c2 ++ "\n")
  putStr ("c3 :" ++ show c3 ++ "\n")
  putStr ( "------SECTION  E ->  S: S----------\n")
  putStr ("a0 :" ++ show a0 ++ "\n")
  putStr ( "------END  K -> + ----------\n")
  putStr ("p1 :" ++ show p1 ++ "\n")
  putStr ("p2 :" ++ show p2 ++ "\n")
  putStr ("p3 :" ++ show p3 ++ "\n")
  putStr ("p4 :" ++ show p4 ++ "\n")
  putStr ("p5 :" ++ show p5 ++ "\n")

