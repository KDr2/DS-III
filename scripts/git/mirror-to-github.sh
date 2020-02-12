#!/bin/bash
# -*- mode: sh -*-

function mirror {
    SOURCE_URL=$1
    TARGET_REPO=$2
    shift 2
    REPO_DIR=$(mktemp -p . -d -u)

    git clone $SOURCE_URL $REPO_DIR
    cd $REPO_DIR

    git remote add mrepo \
        https://KDr2:${REPO_ACCESS_TOKEN}@github.com/KDr2/$TARGET_REPO

    for BRANCH in "$@"; do
        git push mrepo origin/$BRANCH:$BRANCH
    done
}


mirror https://github.com/racket/racket.git racket.git master
