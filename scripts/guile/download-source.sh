#!/bin/bash

URL=https://git.savannah.gnu.org/git/guile.git
# BRANCH=master
DEST=guile

# git clone $URL --depth 1 --branch $BRANCH $DEST
git clone $URL $DEST
