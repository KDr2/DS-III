
LLVM_CONFIG?=llvm-config-3.2

LLVM_CXXFLAGS=$(shell $(LLVM_CONFIG) --cxxflags)
LLVM_CXXFLAGS+=-DLLVM_VERSION=$(shell echo `$(LLVM_CONFIG) --version`| sed s/[^0-9]//g)
LLVM_LDFLAGS+=$(shell $(LLVM_CONFIG) --ldflags)
LLVM_LDFLAGS+=$(shell $(LLVM_CONFIG) --libs all)

CXXFLAGS+=$(LLVM_CXXFLAGS) -g
#LDFLAGS+=-shared

all: s0.exe

s0.exe: sample_0.o
	$(CXX) -Wl,-Bdynamic -o $@ $< $(LDFLAGS) $(LLVM_LDFLAGS)

clean:
	-rm -f *.o *.so
	-rm -f *.exe
