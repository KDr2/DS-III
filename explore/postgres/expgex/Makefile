MODULE_big = expgex
OBJS = expgex.o

EXTENSION = expgex
# DATA = expgex--1.0.sql expgex-2--1.0.sql

REGRESS = expgex

PG_CONFIG ?= pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
