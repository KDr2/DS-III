#!/usr/bin/env bash

cat > /dev/null <<EOF
options:
- --debug: build in DEBUG mode
- -s: sync before building
EOF

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
V8_REPO='https://github.com/v8/v8.git'
V8_SOURCE_DIR=$PWD
V8_SOURCE_REVISON=$(git rev-parse --short HEAD)
V8_SOURCE_BRANCH=$(git rev-parse --abbrev-ref HEAD)
V8_BUILD_DIR="$V8_SOURCE_DIR/out/$V8_SOURCE_BRANCH"
BUILD_MODE='RELEASE'
if [[ $(getopt -q -l debug -- $@) = *--denug* ]]; then
    BUILD_MODE='DEBUG'
fi

# sync code
# gclient config --unmanaged "$PDFium_URL" --name=pdfium
cat > ../v8.gclient <<EOF
solutions = [
  { "name"        : "v8",
    "url"         : "https://github.com/v8/v8.git",
    "deps_file"   : "DEPS",
    "managed"     : False,
    "custom_deps" : {
    },
    "custom_vars": {},
  },
]
EOF

# make the repo clean before sync
git -C $V8_SOURCE_DIR/ checkout BUILD.gn
git -C $V8_SOURCE_DIR/build checkout .

if [[ $(getopt -q -o s -- $@) == *-s* ]]; then
    gclient sync --gclientfile=v8.gclient
fi

mkdir -p "$V8_BUILD_DIR"
# Configure GN args
cat > "$V8_BUILD_DIR/args.gn" <<EOF
is_component_build = false

target_cpu="x64"
# v8_target_cpu="x64"

# monolithic static library
v8_use_external_startup_data = false
v8_monolithic=true
v8_static_library=true

v8_expose_symbols=true

use_goma=false
is_clang=true
EOF


[ "$BUILD_MODE" == "RELEASE" ] && echo 'is_debug = false' >> "$V8_BUILD_DIR/args.gn"
[ "$BUILD_MODE" == "DEBUG" ] && echo 'is_debug = true' >> "$V8_BUILD_DIR/args.gn"


# Generate Ninja files then build
gn gen "$V8_BUILD_DIR"
ninja -C "$V8_BUILD_DIR" d8
# ninja -C "$V8_BUILD_DIR" v8_monolith
