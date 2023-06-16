
my $str = "";
my @links = ();
my %set;

while ($tek = <>){
    $str = $str.$tek;
}

while ($str =~  m{<a(.*?)href=["'](?<link>.*?)["'](.*?)>}gx) {
    push(@links, $+{link}) 
}

for $link (@links) {
    $link =~ m{(//(.*?(:.*?)?@)?(?<f1>.*?)([/#?;:]|$))|((.*?(:.*?)?@)?(?<s2>[^/]*?):(\d+)([/#?;:]|$))}gx;
    
     
    $set{$+{f1}} = 1 if (length($+{f1}) != 0);
    $set{$+{s2}} = 1 if (length($+{s2}) != 0);
}


print "$_\n" for (sort keys(%set));