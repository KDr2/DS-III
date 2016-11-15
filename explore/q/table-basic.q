-1 "with no type, determined after first insert";
t1: ([]c1:(); c2:())
-1 "\nmeta:";
show meta t1
insert[`t1] (`v1; "string");
-1 "\nmeta:";
show meta t1
-1 "\ncontent:";
show t1
\\
