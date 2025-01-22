#!/bin/bash

function __dir__() {
    echo $(dirname $(realpath $0))
}


function string-join() {
    local IFS="$1"
    shift
    echo "$*"
}


#
# arg1: FULL_STRING
# arg2: TARGET_STR_ITEM
# arg3: IFS
# e.g.: string-contains "$PATH" "/bin"
#
function string-contains() {
    local IFS=${3-:}
    local ARRAY=($1)
    for (( i=0; i < ${#ARRAY[*]}; i++ )); do
        [[ $2 == ${ARRAY[i]} ]] && return 0
    done
    return 1
}


#
# arg1: STRING/VAR_NAME
# arg2: TARGET_STRING_ITEM
# arg3: IFS
# e.g.: string-(ap|pre)pend-to-var PATH "/bin"
#

function string-append() {
    local REAL_IFS=${3-:}
    if string-contains "$1" "$2" "$REAL_IFS"; then
        echo "$1"
        return
    fi
    #echo $(string-join "$REAL_IFS" "$FULL_STRING" "$2")
    [[ -z $1 ]] && { echo "$2"; return; }
    echo "$1${REAL_IFS}$2"
}


function string-append-to-var() {
    local REAL_IFS=${3-:}
    local FULL_STRING=$(eval "echo \$$1")
    export $1="$(string-append "$FULL_STRING" "$2" "$REAL_IFS")"
}


function string-prepend() {
    local REAL_IFS=${3-:}
    if string-contains "$1" "$2" "$REAL_IFS"; then
        echo "$1"
        return
    fi
    #echo $(string-join "$REAL_IFS" "$FULL_STRING" "$2")
    [[ -z $1 ]] && { echo "$2"; return; }
    echo "$2${REAL_IFS}$1"
}


function string-prepend-to-var() {
    local REAL_IFS=${3-:}
    local FULL_STRING=$(eval "echo \$$1")
    export $1="$(string-prepend "$FULL_STRING" "$2" "$REAL_IFS")"
}


#
# arg1: STRING/VAR_NAME
# arg2: TARGET_STRING_ITEM
# arg3: IFS
# e.g.: string-remove-from-var PATH "/bin"
#

function string-remove() {
    local REAL_IFS=${3-:}
    if ! string-contains "$1" "$2" "$REAL_IFS"; then
        echo $1
        return
    fi
    local SAVED_IFS="$IFS"
    local IFS="$REAL_IFS"
    local ARRAY=($1)
    IFS="$SAVED_IFS"
    local RET_ARRAY=()
    for (( i=0; i < ${#ARRAY[*]}; i++ )); do
        [[ $2 == ${ARRAY[i]} ]] && continue
        RET_ARRAY[${#RET_ARRAY[*]}]=${ARRAY[i]}
    done
    echo $(string-join "$REAL_IFS" "${RET_ARRAY[@]}")
}


function string-remove-from-var() {
    local REAL_IFS=${3-:}
    local FULL_STRING=$(eval "echo \$$1")
    export $1="$(string-remove "$FULL_STRING" "$2" "$REAL_IFS")"
}
