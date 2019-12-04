#!/bin/bash

URL=https://github.com/llvm-mirror/llvm.git
# BRANCH=master
BRANCH=release_60 # release_70/80/90
DIST=llvm

git clone $URL --depth 1 --branch $BRANCH $DIST
