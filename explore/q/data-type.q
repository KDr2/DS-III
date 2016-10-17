
type_tab: ([t:`symbol$()]; type.id:`int$(); name:`symbol$(); size:`int$(); null.val:`symbol$(); literal:`symbol$())

`type_tab insert (`b;     1; `boolean;  1; `$"";      `$"1b");
`type_tab insert (`x;     4; `byte;     1; `$"";      `$"0xff");
`type_tab insert (`h;     5; `short;    2; `$"0Nh";   `$"23h");
`type_tab insert (`i;     6; `int;      4; `$"0N";    `$"23i");
`type_tab insert (`j;     7; `long;     8; `$"0Nj";   `$"23j");
`type_tab insert (`e;     8; `real;     4; `$"0Ne";   `$"2.3e");
`type_tab insert (`f;     9; `float;    8; `$"0n";    `$"2.3");
`type_tab insert (`c;    10; `char;     1; `$"\" \""; `$"\"a\"");
`type_tab insert (`s;    11; `symbol;   0; `$"`";     `$"`abc");
`type_tab insert (`m;    13; `month;    4; `$"0Nm";   `$"2003.3m");
`type_tab insert (`d;    14; `date;     4; `$"0Nd";   `$"2004.03.27");
`type_tab insert (`z;    15; `datetime; 8; `$"0Nz";   `$"2005.03.27T08:31:53");
`type_tab insert (`u;    17; `minitue;  4; `$"0Nu";   `$"08:31");
`type_tab insert (`v;    18; `second;   4; `$"0Nv";   `$"08:32:53");
`type_tab insert (`t;    19; `time;     4; `$"0Nt";   `$"09:10:35.023");
`type_tab insert (`$"*"; 20; `enum;     4; `$"`s$.."; `$"`s$`b, where s:`a`b");

-1 "-----------------------------------------------------";

show (select from type_tab)

\\
