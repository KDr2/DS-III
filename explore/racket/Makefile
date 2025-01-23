
RACKET = $(PROG_DIR)/racket

all: e3m.exe

module_1.rkt-mod.c: module_1.rkt
	raco ctool --c-mods $@ $<

e3m.exe: e3m.c module_1.rkt-mod.c
	gcc e3m.c -I$(RACKET)/include/racket $(RACKET)/lib/libracket3m.a -ldl -lm -lffi -pthread -o $@
