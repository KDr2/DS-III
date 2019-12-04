#!/bin/bash

URL=https://github.com/llvm-mirror/clang.git
# BRANCH=master
BRANCH=release_60 # release_70/80/90
DIST=clang

git clone $URL --depth 1 --branch $BRANCH $DIST
