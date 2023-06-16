{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module T() where
import Numeric.Natural
import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Char
data Annotated e a = a :# e deriving Show
infix 0 :#

newtype Parser a = P {parse :: String -> (Either ParserError (Annotated String a))}
  deriving newtype (Functor, Applicative, Monad)
newtype ParserError = Err String deriving Show


parseError :: Parser String 
parseError = P (\_ -> (Left (Err "fail")))


satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = P (\cs ->
  case cs of
    (c:cs') ->
      (if predicate c
        then (Right (c:#cs'))
        else (Left (Err "fail")))
    _ -> (Left (Err "fail")))

char :: Char -> Parser Char
char c = satisfy (== c)

data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum
  deriving Show

data Expr = Val Double | Op (Prim Expr) deriving Show
