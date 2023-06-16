$f = 0;
$f1 = 0;
while(<>) {
    s/<[^>]*>//g;
    if (/^\s*$/) {
        if ($f) {$f1 = 1;}
   } else { #stroka ne pustaya
       if ($f1) {print "\n";}
       $f = 1;
       s/^\s+//g;
       s/\s+$//g;
       s/\s+/ /g;
       print;
       $f1 = 0;
       print "\n";
   }
}