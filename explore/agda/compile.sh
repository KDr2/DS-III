#!/bin/bash
if [[ $AGDA == "" ]]; then
    AGDA=agda-2.8.0
fi

$AGDA --compile-dir output --compile $1
