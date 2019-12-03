#!/bin/bash
#
# see https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git
#

URL=https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git
BRANCH=linux-4.19.y
DIST=linux

git clone $URL --depth 1 --branch $BRANCH $DIST
