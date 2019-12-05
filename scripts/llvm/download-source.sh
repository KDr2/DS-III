#!/bin/bash

URL=https://github.com/llvm/llvm-project.git
# BRANCH=master
BRANCH=release/8.x # release/<N>.x
DIST=llvm-project

git clone $URL --depth 1 --branch $BRANCH $DIST
