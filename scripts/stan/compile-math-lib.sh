#!/bin/bash

SOURCE_ROOT=${SOURCE_ROOT-$HOME/Work/hall/stan-math}

# clean
# make -f $SOURCE_ROOT/make/standalone math-clean
make -j4 -f $SOURCE_ROOT/make/standalone math-libs

if [[ -n $1 ]]; then
   make -f $SOURCE_ROOT/make/standalone $1
fi
