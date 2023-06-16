{
module Pa where
import Le
-- dim
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  '#'                { TQuote    } -- аналог кавычки 'some string' == #some string#
  '='                { TEq    } 
  '{'                { TFo    } 
  '}'                {TFc    } 
  '$'                { TDollar    } 
  '['                { TSo    } 
  ']'                { TSc    } 
  ','                { TComma    } 
  '('                { TPo    } 
  ')'                { TPc    } 
  ':'                { TDots    } 
  ';'                { TEnd    } 
  '%'                { TPer    } 
  '|'                { TPiv    } 
  '+'                { TPlus    } 
  '-'                { TMinus    } 
  '*'                { TMul    } 
  '/'                { TDiv    } 
  '^'                { TVoskl    } 
  'right'                { TRight    } 
  'left'                { TLeft    } 
  var                { TVar $$}
  num                { TNum $$}
       
%%


Rules:
  Rule Rules {$1 : $2}
  | Rule {[$1]}

Rule:
  var '=' Per ';'{ Rule $1 $3}

Per: 
  Descr '{' Constr  '}' '|' Per {   (Uno $1 $3) : $6  }
  | Descr '{' Constr  '}'       {    [(Uno $1 $3) ] }
  
Descr: 
  var Descr {(Stroka $1) : $2}
  | Op Descr { $1 : $2}
  | '#''{''#' Descr { (TokStr "{") : $4 }
  | '#''}''#' Descr { (TokStr "}") : $4 }
  | '#''(''#' Descr { (TokStr "(") : $4 }
  | '#'')''#' Descr { (TokStr ")") : $4 }
  | '#'';''#' Descr { (TokStr ";") : $4 }
  | '#''=''#' Descr { (TokStr "=") : $4 }
  | '#'var'#' Descr { (TokStr $2) : $4 }
  | '#'num'#' Descr { (TokInt $2) : $4 }
  | '#''^''#' Descr { (TokStr "^") : $4 }
  | '#''|''#' Descr { (TokStr "|") : $4 }
  | var {[(Stroka $1)]}
  | '#''{''#' {[(TokStr "{")]}
  | '#''}''#' {[(TokStr "}")]}
  | '#''(''#' {[(TokStr "(")]}
  | '#'')''#' {[(TokStr ")")]}
  | '#'';''#' {[(TokStr ";")]}
  | '#''=''#' {[(TokStr "=")]}
  | '#'var'#' {[(TokStr $2)]}
  | '#'num'#' {[(TokInt $2)]}
  | '#''^''#' {[(TokStr "^")]}
  | '#''|''#' {[(TokStr "|")]}
  | Op {[$1]}
    
Constr:
  var Args {Constrname $1 $2}
  | Arg {Inv $1 }
  
Arg:
  '$' num { $2 }  

Args:
  '$' num Args {(Inv $2) : $3}
  | '(' Constr ')' Args { $2 : $4 } 
  | '$' num { [(Inv $2)] } 
  | '(' Constr ')'{ [$2] } 

Op: 
  '+' {Plus}
  | '-' {Minus}
  | '*' {Mul}
  | '/' {Division}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

type Rules = [Rule] 

data Rule = Rule String [Tmp] 
  deriving (Eq, Show)
  
data Constructor = Constrname String [Constructor] | Inv Int
  deriving (Eq, Show)

  
data In = Stroka String | Plus | Minus | Mul | Division | TokInt Int| TokStr String 
  deriving (Eq, Show)

data Tmp = Uno [In] Constructor
  deriving (Eq, Show)
  
  

}