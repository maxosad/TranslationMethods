{
module Lexer where    
}

%wrapper "basic"

$digit=0-9
$alpha=[a-zA-Z]

tokens :-
    $white            ;
   
    \;                { \s -> TEOL    }
    \{                { \s -> TOF    }
    \}                { \s -> TCF    }
    \(                { \s -> TOP    }    
    \)                { \s -> TCP    }
    \+                { \s -> TPlus    }
    \-                { \s -> TMinus   } 
    \*                { \s -> TMul     } 
    \/                { \s -> TDiv     }	

    int              { \s -> TInt     }	
    main             { \s -> TMain     }
    while             { \s -> TWhile     }
    return             { \s -> TReturn     }
    =                { \s -> TEq }

    [$digit]+         { \s -> TConst (read s) }
    $alpha            { \s -> TVar s }

 
    if               {  \s -> TIF       }
    else             {  \s -> TELSE     }
    for             {  \s -> TFor     }
    True             { \s -> TTrue    }
    False            { \s -> TFalse   }
    \>                { \s -> TG       }
    \<                { \s -> TL       }
    \<=               { \s -> TLE       }
    \++               { \s -> TPP       }
    \>=               { \s -> TGE       }
    \==               { \s -> TEQ       }
    \!=               { \s -> TNEQ      }	
{
data Token = TPlus
           | TMinus
           | TReturn
           | TFor
           | TMul
           | TDiv
           | TPP
           | TInt
           | TMain
           | TWhile
           | TOP
           | TCP
           | TOF
           | TCF
           | TEOL
           | TEq
           | TIF
           | TELSE
           | TTrue
           | TFalse
           | TG
           | TL
           | TLE
           | TGE
           | TEQ
           | TNEQ
           | TConst Int
           | TVar String
           deriving (Eq, Show)
}