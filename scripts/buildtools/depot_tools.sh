#!/bin/bash

# setup google's depot_tools

DEPOT_TOOLS_REPO='https://chromium.googlesource.com/chromium/tools/depot_tools.git'
DEPOT_TOOLS_DIR=$PROG_DIR/depot_tools
if [[ ! -d $DEPOT_TOOLS_DIR ]]; then
    git clone "$DEPOT_TOOLS_REPO" "$DEPOT_TOOLS_DIR"
else
    git -C "$DEPOT_TOOLS_DIR" pull
fi
