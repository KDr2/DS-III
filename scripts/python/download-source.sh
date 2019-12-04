#!/bin/bash

URL=https://github.com/python/cpython.git
# BRANCH=master
BRANCH=3.5 # 3.6, 3.7, 3.8
DIST=cpython

git clone $URL --depth 1 --branch $BRANCH $DIST
