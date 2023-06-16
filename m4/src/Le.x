{
module Le where 
-- dim   
}

%wrapper "basic"

$digit=0-9
$alpha=[a-zA-Z]
tokens :-
    $white                             ;
    "#"                { \s -> TQuote    } 
    "["                { \s -> TSo    } 
    "]"                { \s -> TSc    } 
    "("                { \s -> TPo    } 
    ")"                { \s -> TPc    } 
    ","                { \s -> TComma    } 
    "="                { \s -> TEq    } 
    "{"                { \s -> TFo    } 
    "}"                { \s -> TFc    } 
    "$"                { \s -> TDollar    } 
    ":"                { \s -> TDots    } 
    ";"                { \s -> TEnd    } 
    "%"                { \s -> TPer    } 
    "|"                { \s -> TPiv    } 
    "+"                { \s -> TPlus    } 
    "-"                { \s -> TMinus    } 
    "*"                { \s -> TMul    } 
    "/"                { \s -> TDiv    } 
    "^"                { \s -> TVoskl    } 
    "right"                { \s -> TRight    } 
    "left"                { \s -> TLeft    } 
    $alpha+            { \s -> TVar s }
    $digit+         {  \s -> TNum (read s) }

{
data Token = TDots 
           | TQuote
           | TVoskl
           | TPo
           | TPc
           | TSo
           | TSc
           | TComma
           | TEq
           | TFo
           | TFc
           | TDollar
           | TEnd
           | TPer
           | TPlus
           | TPiv
           | TMinus
           | TMul
           | TDiv
           | TLeft
           | TRight
           | TVar String
           | TNum Int
           deriving (Eq, Show)
}