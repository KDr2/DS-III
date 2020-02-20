#!/bin/bash
# -*- mode: sh -*-

function github_repo {
    GITHUB_USER=$1
    TARGET_REPO=$2
    if [[ "$TARGET_REPO" != */* ]]; then
        TARGET_REPO=${GITHUB_USER}/${TARGET_REPO}
    fi
    echo https://${GITHUB_USER}:${REPO_ACCESS_TOKEN}@github.com/${TARGET_REPO}
}

function mirror {
    SOURCE_URL=$1
    TARGET_URL=$2
    shift 2
    REPO_DIR=$(mktemp -p . -d -u)

    git clone $SOURCE_URL $REPO_DIR
    cd $REPO_DIR

    git remote add mrepo $TARGET_URL

    # push specified branches
    for BRANCH in "$@"; do
        git push mrepo origin/$BRANCH:$BRANCH
    done

    # push all tags
    for TAG in $(git tag); do
        git push mrepo $TAG
    done
}

# Julia
mirror https://github.com/JuliaLang/julia.git $(github_repo KDr2 julia.git) master
mirror https://github.com/FluxML/Flux.jl.git $(github_repo KDr2 Flux.jl.git) master

# Database
mirror https://github.com/postgres/postgres.git $(github_repo KDr2 postgres.git) master

# Google
mirror https://github.com/google/skia.git $(github_repo KDr2 skia.git) master
mirror https://pdfium.googlesource.com/pdfium $(github_repo KDr2 PDFium.git) master

# Lisp
mirror https://github.com/racket/racket.git $(github_repo KDr2 racket.git) master
mirror https://git.savannah.gnu.org/git/guile.git $(github_repo KDr2 guile.git) master
mirror https://git.code.sf.net/p/sbcl/sbcl $(github_repo KDr2 SBCL.git) master
mirror https://gitlab.com/embeddable-common-lisp/ecl.git $(github_repo KDr2 ECL.git) develop master # develop is the main master
# mirror https://git.savannah.gnu.org/git/emacs.git $(github_repo KDr2 emacs.git) master emacs-27
mirror https://code.orgmode.org/bzg/org-mode.git $(github_repo KDr2 org-mode.git) master maint
