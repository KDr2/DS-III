CXX ?= g++

PDFIUM_ROOT ?= $(HOME)/Work/pai/pdfium
PDFIUM_BRANCH ?= $(shell git -C $(PDFIUM_ROOT) rev-parse --abbrev-ref HEAD)

# include paths
INC += -I$(PDFIUM_ROOT)
INC += -I$(PDFIUM_ROOT)/third_party/freetype/src/include
INC += -I$(PDFIUM_ROOT)/third_party/abseil-cpp

# library paths
LIBSPATH =  -L$(PDFIUM_ROOT)/out/$(PDFIUM_BRANCH)
LIBSPATH += -L$(PDFIUM_ROOT)/out/$(PDFIUM_BRANCH)/obj
LIBS = -lpdfium -ldl -lpthread -lz -lexpat -lpng -ljpeg

# compiler flags
CFLAGS += -std=c++14 -Wall $(INC)

ifneq (,$(DEBUG))
	CFLAGS += -g -O0 -D_GLIBCXX_DEBUG
endif

ifeq (mu,$(PDFIUM_BRANCH))
	CFLAGS += -DEXT_MU=1
else ifeq (bud,$(PDFIUM_BRANCH))
	CFLAGS += -DEXT_BUD=1
endif

# rules

OBJS = common_utils.o

%.o: %.cpp %.h
	$(CXX) $(CFLAGS) -fPIC -g -c $< -o $@
%.o: %.cpp
	$(CXX) $(CFLAGS) -fPIC -g -c $< -o $@

exe-%: $(OBJS) main-%.o
	$(CXX) -o $@ $^ $(LIBSPATH) $(LIBS)

all:
	@echo "Usage: make exe-<name>"

clean:
	rm -f *.o
	rm -f exe-*
