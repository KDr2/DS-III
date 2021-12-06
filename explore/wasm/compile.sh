#!/bin/bash

if [[ $1 == "thin" ]]; then
    # standalone:
    em++ -std=c++11 interact.cpp  -Os -s WASM -o thin.wasm --js-library interact-lib.js
else
    # with html+js
    em++ -std=c++11 interact.cpp  -O0 -s WASM -o fat.waso.html --js-library interact-lib.js \
         -DFAT -s EXPORTED_FUNCTIONS="['_free', '_main', '_malloc']"
fi
