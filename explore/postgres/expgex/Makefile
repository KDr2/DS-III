MODULE_big = expgex
OBJS = expgex.o
# LDFLAGS = exs.a

# SHLIB_LINK = y/y.a

EXTENSION = expgex
# DATA = expgex--1.0.sql expgex-2--1.0.sql

REGRESS = expgex

PG_CONFIG ?= /data/kdr2/db/pgsql-dev/bin/pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
