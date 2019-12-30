#!/bin/bash

URL=https://github.com/Perl/perl5.git
# BRANCH=blead
BRANCH=maint-5.22
DEST=perl5

git clone $URL --depth 1 --branch $BRANCH $DEST
