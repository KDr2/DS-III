#!/bin/bash
#
# see:
#   - http://savannah.gnu.org/git/?group=emacs
#   - http://git.savannah.gnu.org/cgit/emacs.git
#

URL=https://git.savannah.gnu.org/git/emacs.git
# BRANCH=master
BRANCH=emacs-26 # emacs-25
DIST=emacs

git clone $URL --depth 1 --branch $BRANCH $DIST
