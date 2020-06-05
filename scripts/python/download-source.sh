#!/bin/bash

URL=https://github.com/python/cpython.git
# BRANCH=master
BRANCH=3.8 # 3.6, 3.7, 3.8
DEST=cpython

git clone $URL --depth 1 --branch $BRANCH $DEST
