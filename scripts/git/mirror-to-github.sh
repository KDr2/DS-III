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

mirror https://github.com/JuliaLang/julia.git julia.git master
mirror https://github.com/FluxML/Flux.jl.git Flux.jl.git master

mirror https://github.com/postgres/postgres.git postgres.git master

mirror https://github.com/google/skia.git skia.git master
mirror https://pdfium.googlesource.com/pdfium PDFium.git master

mirror https://code.orgmode.org/bzg/org-mode.git org-mode.git master

mirror https://github.com/racket/racket.git racket.git master
