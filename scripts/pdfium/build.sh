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

# determine the directory
if [[ -d public && -d core && -d fpdfsdk ]]; then
    TOP_DIR=$PWD/..
    PDFium_SOURCE_DIR=$PWD
else
    TOP_DIR=$PWD
    PDFium_SOURCE_DIR=$PWD/pdfium
fi

# Input
PDFium_SOURCE_REVIISON=unknown
CONFIGURATION='Release'

PDFium_URL='https://github.com/KDr2/PDFium.git' # or git@github.com:KDr2/PDFium.git
DepotTools_URL='https://chromium.googlesource.com/chromium/tools/depot_tools.git'
DepotTools_DIR="$TOP_DIR/depot_tools"

PDFium_BUILD_DIR="$PDFium_SOURCE_DIR/out/Default"
PDFium_CI_DIR="$PDFium_SOURCE_DIR/ci-build"
PDFium_CMAKE_CONFIG="$PDFium_CI_DIR/PDFiumConfig.cmake"
PDFium_ARGS="$PDFium_CI_DIR/args/$OS.args.gn"

# Output
PDFium_STAGING_DIR="$TOP_DIR/lib-pdfium"
PDFium_INCLUDE_DIR="$PDFium_STAGING_DIR/include"
PDFium_LIB_DIR="$PDFium_STAGING_DIR/lib"

# Download depot_tools
if [ ! -e "$DepotTools_DIR" ]; then
    git clone "$DepotTools_URL" "$DepotTools_DIR"
else
    :
    # git -C "$DepotTools_DIR" pull
fi
export PATH="$DepotTools_DIR:$PATH"

# Clone
if [ ! -e "$PDFium_SOURCE_DIR" ]; then
    gclient config --unmanaged "$PDFium_URL" --name=pdfium
fi
# gclient sync

# Prepare directories
mkdir -p "$PDFium_BUILD_DIR"
mkdir -p "$PDFium_STAGING_DIR"
mkdir -p "$PDFium_LIB_DIR"

# Checkout to target branch
cd "$PDFium_SOURCE_DIR"
PDFium_SOURCE_REVIISON=$(git rev-parse --short HEAD)
# gclient sync

# Configure
cat > "$PDFium_BUILD_DIR/args.gn" <<EOF
is_component_build = false
pdf_enable_v8 = false
pdf_is_standalone = true
use_custom_libcxx=true
EOF

[ "$CONFIGURATION" == "Release" ] && echo 'is_debug=false' >> "$PDFium_BUILD_DIR/args.gn"

# Generate Ninja files
gn gen "$PDFium_BUILD_DIR"

# Build
ninja -C "$PDFium_BUILD_DIR" pdfium

# Install
cp "$PDFium_SOURCE_DIR/LICENSE" "$PDFium_STAGING_DIR"
rm -rf "$PDFium_INCLUDE_DIR"
cp -R "$PDFium_SOURCE_DIR/public" "$PDFium_INCLUDE_DIR"
rm -f "$PDFium_INCLUDE_DIR/DEPS"
rm -f "$PDFium_INCLUDE_DIR/README"
rm -f "$PDFium_INCLUDE_DIR/PRESUBMIT.py"
[ "$OS" == "linux" ] && mv "$PDFium_BUILD_DIR/libpdfium.so" "$PDFium_LIB_DIR"
[ "$OS" == "darwin" ] && mv "$PDFium_BUILD_DIR/libpdfium.dylib" "$PDFium_LIB_DIR"
