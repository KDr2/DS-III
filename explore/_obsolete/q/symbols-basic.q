-1 "()";

-1 "@"; / apply
show @[til 10;(::;2 3);2+]
show @[til 10;(::; 2 3; 2 3);2+]

f1: {base:(-1#x)[0]; (base - -1_x)%base}
f2: {max (max':) (f1':)  (neg til count a) (_) \: a}

\\
