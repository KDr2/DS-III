#!/usr/bin/env bash

cat > /dev/null <<EOF
options:
- -b|--branch: branch to build
- --shared: build shared lib
- --static: build static lib
- --skia: build with skia
- -s|--sync: sync before building
- --debug: build in DEBUG mode
EOF

# set -ex

### OPTIONS

OPT_BRANCH=-
OPT_SHARED=YES
OPT_STATIC=NO
OPT_SKIA=NO
OPT_CHECKOUT=NO
OPT_SYNC=NO
OPT_DEBUG=NO

POSITIONAL_ARGS=()
while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
        -b|--branch)
            OPT_BRANCH="$2"
            shift # past argument
            shift # past value
            ;;
        --share|--shared)
            OPT_SHARED=YES
            OPT_STATIC=NO
            shift # past argument
            ;;
        --static)
            OPT_SHARED=NO
            OPT_STATIC=YES
            # SEARCHPATH="$2"
            shift # past argument
            ;;
        --skia)
            OPT_SKIA=YES
            shift # past argument
            ;;
        --checkout)
            OPT_CHECKOUT=YES
            shift # past argument
            ;;
        -s|--sync)
            OPT_SYNC=YES
            shift # past argument
            ;;
        --debug)
            OPT_DEBUG=YES
            shift # past argument
            ;;
        *)  # unknown option
            POSITIONAL_ARGS+=("$1") # save it in an array for later
            echo "Unknow option: $1"
            shift # past argument
            ;;
    esac
done

## OS name
OS=$(uname)
case $OS in
    MINGW*)
        OS="windows"
        ;;
    *)
        OS=$(echo $OS | tr '[:upper:]' '[:lower:]')
        ;;
esac

# Input
PDFIUM_REPO='https://github.com/PaodingAI/pdfium.git'
PDFIUM_SOURCE_DIR=$PWD
PDFIUM_SOURCE_REVISON=$(git rev-parse --short HEAD)
PDFIUM_SOURCE_BRANCH=$(git rev-parse --abbrev-ref HEAD)
BUILD_MODE='RELEASE'
if [[ $OPT_DEBUG == YES ]]; then
    BUILD_MODE='DEBUG'
fi

# sync code
# gclient config --unmanaged "$PDFium_URL" --name=pdfium
cat > ../pdfium.gclient <<EOF
solutions = [
  { "name"        : "pdfium",
    "url"         : "git@github.com:PaodingAI/pdfium.git",
    "deps_file"   : "DEPS",
    "managed"     : False,
    "custom_deps" : {
    },
    "custom_vars": {},
  },
]
EOF

# make the repo clean before sync
if [[ $OPT_CHECKOUT == YES ]]; then
    git -C $PDFIUM_SOURCE_DIR/ checkout BUILD.gn
    [[ -d $PDFIUM_SOURCE_DIR/build ]] && git -C $PDFIUM_SOURCE_DIR/build checkout .
fi

if [[ $OPT_BRANCH != - ]]; then
    PDFIUM_SOURCE_BRANCH=$OPT_BRANCH
    git checkout $OPT_BRANCH
fi
PDFIUM_BUILD_DIR="$PDFIUM_SOURCE_DIR/out/$PDFIUM_SOURCE_BRANCH"
echo building branch [$PDFIUM_SOURCE_BRANCH]...


if [[ $OPT_SYNC == YES ]]; then
    gclient sync --gclientfile=pdfium.gclient
fi

# set visibility to default
perl -p -i.bak \
     -e 's#//build/config/gcc:symbol_visibility_hidden#//build/config/gcc:symbol_visibility_default#gi' \
     $PDFIUM_SOURCE_DIR/build/config/BUILDCONFIG.gn

if [[ $OPT_SHARED == YES ]]; then
    perl -p -i.bak \
         -e 's#component\("pdfium"\) \{#shared_library("pdfium") \{#gi' \
         $PDFIUM_SOURCE_DIR/BUILD.gn
fi

mkdir -p "$PDFIUM_BUILD_DIR"
# Configure GN args
cat > "${PDFIUM_BUILD_DIR}/args.gn" <<EOF
is_component_build = false
pdf_enable_v8 = false
pdf_is_standalone = true
use_custom_libcxx = false
# is_clang = false
# use_sysroot = false
EOF

if [[ $OPT_SKIA == YES ]]; then
    # use cxx14, for skia
    echo 'use_cxx11 = false' >> "$PDFIUM_BUILD_DIR/args.gn"
    echo 'pdf_use_skia = true' >> "$PDFIUM_BUILD_DIR/args.gn"
fi

if [[ $OPT_STATIC == YES ]]; then
    echo 'pdf_is_complete_lib = true' >> "$PDFIUM_BUILD_DIR/args.gn"
fi

[ "$BUILD_MODE" == "RELEASE" ] && echo 'is_debug = false' >> "$PDFIUM_BUILD_DIR/args.gn"
[ "$BUILD_MODE" == "DEBUG" ] && echo 'is_debug = true' >> "$PDFIUM_BUILD_DIR/args.gn"


# Generate Ninja files then build
gn gen "$PDFIUM_BUILD_DIR"
ninja -C "$PDFIUM_BUILD_DIR" pdfium
