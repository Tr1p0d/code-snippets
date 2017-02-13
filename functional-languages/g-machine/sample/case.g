main = case Pack{1,2} 2 3 of
    <0> x y -> 12345;
    <1> x y -> K y x
