changecom(`%')dnl
dnl
define(lc, `translit(`$1', `A-Z', `a-z')')dnl
define(ifos, `ifelse(lc(patsubst(esyscmd(uname), `\s+', `')), lc($1), `$2', `')')dnl
define(dft, `ifdef(`$1', `', `define(`$1',$2)')')dnl
dnl
dnl
dft(`FontMain', Simsun)dnl
dft(`FontSans', Simsun)dnl
dnl
ifos(linux, `define(`FontMain', AR PL SungtiL GB)')dnl ###
ifos(linux, `define(`FontItalic', AR PL KaitiM GB)')dnl %aa
ifos(linux, `define(`FontSans', WenQuanYi Zen Hei)')dnl
dnl
ifos(darwin, `define(`FontMain', STFangsong)')dnl ###
ifos(darwin, `define(`FontItalic', STFangsong)')dnl %aa
ifos(darwin, `define(`FontSans', Monaco)')dnl
dnl
dnl
\setCJKmainfont[ItalicFont={FontItalic}]{FontMain}
\setCJKsansfont{FontSans}
