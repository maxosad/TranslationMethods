module Lab2.L                    
(                    
)
where 

import Lab2.Parser 
import HW2.T6
import HW2.T1 (Except(..))



mainexpr = pars

fromSucc (Success s) = s 

mepr = pars
e0 = runP mepr "((aaa*a|a)*aa(aa|a*)a)*" -- In Example E ->  S: S
e1 = runP mepr "((a*a|a)*a(a|a*)a)*"

s1 = runP mepr "a*" -- S -> E1 K
s2 = runP mepr "(a*)"
s3 = runP mepr "(a)*"
s4 = runP mepr "(a)**"

c0 = runP mepr "a|a" -- E -> S | S
c1 = runP mepr "a*|*a*"
c2 = runP mepr "a*|a*"
c3 = runP mepr "a*|"

a0 = runP mepr "aaaaa" -- E ->  S: S

p1 = runP mepr "aa(aa)*+a"
p2 = runP mepr "(a+a+)+a"
p3 = runP mepr "a+|"
p4 = runP mepr "a+|a*"
p5 = runP mepr "a+a+a+"

main = do 
  putStr ( "------START----------\n")
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




