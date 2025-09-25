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

    echo "Mirroring [$SOURCE_URL] to [$TARGET_URL]..."

    git clone $SOURCE_URL $REPO_DIR
    cd $REPO_DIR
    if [[ $? -ne 0 ]]; then
        echo "### !!! Failed to clone [$SOURCE_URL]."
        return
    fi

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

            if [[ $? -ne 0 ]]; then
                git push mrepo origin/$BRANCH:$BRANCH -f
            fi
        fi
    done

    # push all tags
    #   We don't use `push --tags` because we don't want to push all tags, and
    #   push some tags will invite an error (e.g. in the SBCL repo)

    git push mrepo --tags

    # for TAG in $(git tag); do
    #     NUMBERS=$(echo $TAG | perl -p -e 's/\D//g')
    #     # try to eliminate non-version tags
    #     if [[ (${#TAG} -gt 24) || (${#NUMBERS} -lt 3) || ${TAG} = *rc* ]]; then
    #         continue
    #     fi
    #     git push -f mrepo $TAG
    # done
}


### Compiler + Lang
if [[ $1 == 'lang-1' ]]; then
    mirror https://github.com/rust-lang/rust.git $(github_repo KDr2 rust.git) master stable
    mirror https://github.com/JuliaLang/julia.git $(github_repo KDr2 julia.git) master
fi

if [[ $1 == 'lang-2' ]]; then
    mirror https://github.com/ghc/ghc.git $(github_repo KDr2 ghc.git) master
    mirror https://github.com/agda/agda.git $(github_repo KDr2 agda.git) master
    mirror https://github.com/agda/agda-stdlib.git $(github_repo KDr2 agda-stdlib.git) master
fi

### Others
if [[ $1 == 'others' ]]; then
    # ML
    mirror https://github.com/pytorch/pytorch.git $(github_repo KDr2 pytorch.git) main

    # Layout and Renderer
    mirror https://github.com/typst/typst.git $(github_repo KDr2 typst.git) main

    # TODO: remove PDFium
    mirror https://pdfium.googlesource.com/pdfium $(github_repo PaodingAI pdfium.git) main
fi
