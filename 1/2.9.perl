while(<>) {
    s/\(.*?\)/()/g;
    print;
}