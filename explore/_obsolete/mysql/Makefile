all: udf_utils

MYSQL_SRC ?= /home/kdr2/Work/opensrc/mysql-server
MYSQL_INC_OPT = -I$(MYSQL_SRC)/include -I$(MYSQL_SRC)/sql -I$(MYSQL_SRC)/regex -I$(MYSQL_SRC)/build/include

udf_utils: udf_utils.cc
	g++ -fPIC -shared -o $(<:%.cc=%.so)  $< $(MYSQL_INC_OPT)
