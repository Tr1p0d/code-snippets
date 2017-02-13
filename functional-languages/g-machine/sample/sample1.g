id = S K K;

main = case Pack{0,1} 123 456 of
    <0> x y -> id x;
    <1> x y -> id y
