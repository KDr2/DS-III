PKG_CONFIG_PATH = $(HOME)/programs/guile/lib/pkgconfig
# export PKG_CONFIG_PATH

all: e1.exe

e1.exe: e1.c
	gcc -o $@ $< $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --cflags --libs guile-3.0)

clean:
	@rm -f *.exe
