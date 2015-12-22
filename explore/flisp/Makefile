
FLISP?=$(HOME)/Work/opensrc/femtolisp

all: raw

flisp_dissect.boot: boot_dissect.lsp
	./flisp $(FLISP)/mkboot0.lsp $< > $@

dissect: flisp_dissect.boot
	gcc -I$(FLISP) -I$(FLISP)/llt -c fl_dissect.c -o fl_dissect.o
	gcc -o fl_dissect fl_dissect.o -lm $(FLISP)/libflisp.a $(FLISP)/llt/libllt.a

flisp_raw.boot: boot_raw.lsp
	./flisp $(FLISP)/mkboot0.lsp $< > $@

raw: flisp_raw.boot
	gcc -I$(FLISP) -I$(FLISP)/llt -c fl_raw.c -o fl_raw.o
	gcc -o fl_raw fl_raw.o -lm $(FLISP)/libflisp.a $(FLISP)/llt/libllt.a


flisp.boot.inc: $(FLISP)/flisp.boot
	./flisp ./bin2hex.scm < $< > $@

standalone: flisp.boot.inc
	gcc -I$(FLISP) -I$(FLISP)/llt -c fl_standalone.c -o fl_standalone.o
	gcc -o fl_standalone fl_standalone.o -lm $(FLISP)/libflisp.a $(FLISP)/llt/libllt.a

clean:
	@rm -f flraw
	@rm -f fl_standalone
	@rm -f flisp.boot.inc
	@rm -f *.o
