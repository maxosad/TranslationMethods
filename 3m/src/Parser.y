{
module Parser where
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
        '+' { TPlus }
        '++' { TPP }
        '-' { TMinus }
        '*' { TMul }
        '/' { TDiv }
        
        '{' { TOF }
        '}' { TCF }
		
        '(' { TOP }
        ')' { TCP }
        
        ';' { TEOL }
        
        '=' { TEq }
        
        'int' {TInt}
        'main' {TMain}
        
        'while' { TWhile }
        'for' { TFor }
        'return' { TReturn }
        'if'    { TIF }
        'else'  { TELSE }
        'True'  { TTrue }
        'False' { TFalse }
        '>'     { TG }
        '<'     { TL }
        '<='    { TLE }
        '>='    { TGE }
        '=='    { TEQ }
        '!='    { TNEQ }
		
		cnst { TConst $$}
        var     { TVar $$}
        
%%
Root:
  'int' 'main' '(' ')' Block  {RootBlock $5}


Block:
  '{' Text '}' {Blck $2}
  
  

Text:
  Str ';'  {Line $1}
  | Str ';' Text  {Lines $1 $3}
  | If Text  {LinesIf $1 $2}
  | If  {LineIf $1 }
  | While Text  {LinesWhile $1 $2}
  | While  {LineWhile $1 }
  | For Text  {LinesFor $1 $2}
  | For  {LineFor $1 }


Str: 
  'int' var '=' Arithm {Assign $2 $4}
  | 'int' var          {Declar $2}
  | var '=' Arithm         {Set $1 $3}
  | 'return' Arithm         {Return $2}


Arithm:
  Arithm '+' Arithm   { $1 :+: $3 }
  | Arithm '-' Arithm   { $1 :-: $3 }
  | Arithm '*' Arithm   { $1 :*: $3 }
  | Arithm '/' Arithm   { $1 :/: $3 }
  | Unary             { $1 }
	
Unary:
    cnst                  { Const $1 } 
    | var                 { Var   $1 }

LogicUnary:
    'True'              { BoolValue True }
    | 'False'            { BoolValue False }

Cmp:
    Arithm '<'  Arithm      { $1 :<: $3 }
    | Arithm '>'  Arithm    { $1 :>: $3 }
    | Arithm '>='  Arithm   { $1 :>=: $3 }
    | Arithm '<='  Arithm   { $1 :<=: $3 }
    | Arithm '=='  Arithm   { $1 :==: $3 }
    | Arithm '!='  Arithm   { $1 :!=: $3 }

Logic:
    Cmp                 { $1        }
    | LogicUnary        { $1        }	


For: 
    'for' '(' 'int' var '=' Arithm ';' Logic ';' var '++' ')' Block {ForExpr $4 $6 $8 $10 $13}

While:
    'while' '(' Logic ')' Block {WhileExpr $3 $5}

If:
    'if' '(' Logic ')' Block    { IfExpr $3 $5  }
    | 'if' '(' Logic ')' Block 'else' Block { IfElExpr $3 $5 $7 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

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
 
data ArithmExpr = ArithmExpr :+: ArithmExpr 
                | ArithmExpr :-: ArithmExpr  
                | ArithmExpr :*: ArithmExpr 
                | ArithmExpr :/: ArithmExpr 
                | Const Int
                | Var String				
				deriving (Eq)
				
data Str = Assign String ArithmExpr 
         | Declar String 
         | Set String ArithmExpr
         | Return ArithmExpr
		 deriving (Eq)

data Root = RootBlock Block	
  deriving (Eq)

data LogicExpr = BoolValue Bool 
               | ArithmExpr :<: ArithmExpr
               | ArithmExpr :>: ArithmExpr
               | ArithmExpr :>=: ArithmExpr
               | ArithmExpr :<=: ArithmExpr
               | ArithmExpr :==: ArithmExpr
               | ArithmExpr :!=: ArithmExpr
               deriving (Eq)

data IfExpr = IfExpr LogicExpr Block
            | IfElExpr LogicExpr Block Block
           	deriving (Eq)

data WhileExpr = WhileExpr LogicExpr Block
  deriving (Eq)	


data ForExpr = ForExpr String ArithmExpr LogicExpr String Block
  deriving (Eq)	


  
}