
PYTHON_CONFIG ?= /usr/bin/python-config
PY_FLAGS = $(shell $(PYTHON_CONFIG) --cflags)

bst_py0.so: bst_py0.cpp
	g++ -shared -fPIC $(PY_FLAGS) -lboost_python $< -o $@

clean:
	rm -fr *.so
