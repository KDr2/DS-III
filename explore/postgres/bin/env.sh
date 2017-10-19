#!/bin/bash

BIN_DIR=$(dirname $0)
PROJECT_ROOT=$(perl -mCwd -e "print Cwd::abs_path('$BIN_DIR/..')")
DATA_DIR=${PROJECT_ROOT}/data/dev-data
USER=${1:-postgres}
DB_NAME=${2:-expg}
PORT=${3:-34567}
DB_HOST=${4:-localhost}

PG_ROOT_DIRS=(
    /data/kdr2/db/pgsql-dev
    /usr/local/pgsql/bin
    /usr/lib/postgresql/9.6
)

for PG_DIR in ${PG_ROOT_DIRS[@]}; do
    echo $PG_DIR
    if [ -d $PG_DIR/bin ]; then
        export PATH=$PG_DIR/bin:$PATH
        break
    fi
done

mkdir -p ${DATA_DIR}
