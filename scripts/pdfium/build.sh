#!/usr/bin/env bash

set -ex

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
PDFium_REPO='https://github.com/KDr2/PDFium.git' # or git@github.com:KDr2/PDFium.git
PDFium_SOURCE_DIR=$PWD
PDFium_SOURCE_REVISON=$(git rev-parse --short HEAD)
PDFium_SOURCE_BRANCH=$(git rev-parse --abbrev-ref HEAD)
PDFium_BUILD_DIR="$PDFium_SOURCE_DIR/out/$PDFium_SOURCE_BRANCH"
CONFIGURATION='Release'
if [[ $(getopt -q d $@) = *-d* ]]; then
    CONFIGURATION='Debug'
fi

# sync code
# gclient config --unmanaged "$PDFium_URL" --name=pdfium
cat > ../pdfium.gclient <<EOF
solutions = [
  { "name"        : "pdfium",
    "url"         : "git@github.com:KDr2/PDFium.git",
    "deps_file"   : "DEPS",
    "managed"     : False,
    "custom_deps" : {
    },
    "custom_vars": {},
  },
]
EOF
if [[ $(getopt -q s $@) == *-s* ]]; then
    gclient sync --gclientfile=pdfium.gclient
fi

perl -p -i.bak \
     -e 's#//build/config/gcc:symbol_visibility_hidden#//build/config/gcc:symbol_visibility_default#gi' \
     $PDFium_SOURCE_DIR/build/config/BUILDCONFIG.gn

mkdir -p "$PDFium_BUILD_DIR"
# Configure GN args
cat > "$PDFium_BUILD_DIR/args.gn" <<EOF
is_component_build = false
pdf_enable_v8 = false
pdf_is_standalone = true
use_custom_libcxx = false
is_clang = false
# use_sysroot = false
EOF

[ "$CONFIGURATION" == "Release" ] && echo 'is_debug=false' >> "$PDFium_BUILD_DIR/args.gn"

# Generate Ninja files then build
gn gen "$PDFium_BUILD_DIR"
ninja -C "$PDFium_BUILD_DIR" pdfium

# ====== #
exit 0   #
# ====== #

# Output
PDFium_STAGING_DIR="$PDFium_SOURCE_DIR/../lib-pdfium"
PDFium_INCLUDE_DIR="$PDFium_STAGING_DIR/include"
PDFium_LIB_DIR="$PDFium_STAGING_DIR/lib"
mkdir -p "$PDFium_STAGING_DIR"
mkdir -p "$PDFium_LIB_DIR"

# Install
cp "$PDFium_SOURCE_DIR/LICENSE" "$PDFium_STAGING_DIR"
rm -rf "$PDFium_INCLUDE_DIR"
cp -R "$PDFium_SOURCE_DIR/public" "$PDFium_INCLUDE_DIR"
rm -f "$PDFium_INCLUDE_DIR/DEPS"
rm -f "$PDFium_INCLUDE_DIR/README"
rm -f "$PDFium_INCLUDE_DIR/PRESUBMIT.py"
[ "$OS" == "linux" ] && cp "$PDFium_BUILD_DIR/libpdfium.so" "$PDFium_LIB_DIR"
[ "$OS" == "darwin" ] && cp "$PDFium_BUILD_DIR/libpdfium.dylib" "$PDFium_LIB_DIR"
