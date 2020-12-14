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

    echo "Mirroring [$SOURCE_URL] to [$TARGET_URL]"

    git clone $SOURCE_URL $REPO_DIR
    cd $REPO_DIR

    git remote add mrepo $TARGET_URL

    # push specified branches
    for BRANCH in "$@"; do
        git push mrepo origin/$BRANCH:$BRANCH
        if [[ $? -ne 0 ]]; then
            CUR_BRANCH=$(git rev-parse --abbrev-ref HEAD)
            if [[ $CUR_BRANCH != $BRANCH ]]; then
                git checkout -b $BRANCH origin/$BRANCH
            fi
            git push mrepo $BRANCH
        fi
    done

    # push all tags
    #   We don't use `push --tags` because we don't want to push all tags, and
    #   push some tags will invite an error (in the SBCL repo)
    for TAG in $(git tag); do
        NUMBERS=$(echo $TAG | perl -p -e 's/\D//g')
        # try to eliminate non-version tags
        if [[ (${#TAG} -gt 24) || (${#NUMBERS} -lt 3) || ${TAG} = *rc* ]]; then
            continue
        fi
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
mirror https://pdfium.googlesource.com/pdfium $(github_repo KDr2 pdfium.git) master

# Lisp
mirror https://github.com/racket/racket.git $(github_repo KDr2 racket.git) master
# mirror https://git.savannah.gnu.org/git/guile.git $(github_repo KDr2 guile.git) master
mirror https://git.code.sf.net/p/sbcl/sbcl $(github_repo KDr2 SBCL.git) master
mirror https://gitlab.com/embeddable-common-lisp/ecl.git $(github_repo KDr2 ECL.git) develop master # develop is the main master
mirror https://git.savannah.gnu.org/git/emacs.git $(github_repo KDr2 emacs.git) master emacs-27
mirror https://code.orgmode.org/bzg/org-mode.git $(github_repo KDr2 org-mode.git) master maint

# ML
mirror https://github.com/pytorch/pytorch.git $(github_repo KDr2 pytorch.git) master
mirror https://github.com/stan-dev/math.git $(github_repo KDr2 stan-math.git) develop
