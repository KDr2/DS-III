#
#
#

LUAJIT_SRC_ROOT?=$(HOME)/Work/opensrc/lua
DASM=$(LUAJIT_SRC_ROOT)/src/luajit $(LUAJIT_SRC_ROOT)/dynasm/dynasm.lua
CFLAGS+=-I$(LUAJIT_SRC_ROOT)

EXECUTABLES = dasm-sample

all: $(EXECUTABLES)

dasm-sample: sample.dasc dynasm-driver.c
	$(DASM) -o sample.dasc.c sample.dasc
	$(CC) $(CFLAGS) -o $@ -DJIT=\"sample.dasc.c\" dynasm-driver.c

clean:
	rm -f $(EXECUTABLES)
	rm -f *.o
	rm -f *.dasc.c *.dasc.h
