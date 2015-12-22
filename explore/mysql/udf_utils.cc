#include <my_global.h>
#include <my_sys.h>

#include <new>
#include <vector>
#include <algorithm>

#define MYSQL_SERVER

#if defined(MYSQL_SERVER)
#include <m_string.h>		/* To get strmov() */
#else
/* when compiled as standalone */
#include <string.h>
#define strmov(a,b) stpcpy(a,b)
#endif

#include <mysqld.h>
#include <sql_class.h>

#include <mysql.h>
#include <ctype.h>

#include <sys/types.h>
#include <unistd.h>

#ifdef HAVE_DLOPEN

// create function my_getpid returns integer soname "udf_utils.so";

C_MODE_START;
my_bool my_getpid_init(UDF_INIT *initid, UDF_ARGS *args, char *message);
void my_getpid_deinit(UDF_INIT *initid);
const char* my_getpid(UDF_INIT *initid, UDF_ARGS *args, char *result,
	       unsigned long *length, char *is_null, char *error);
C_MODE_END;

my_bool my_getpid_init(UDF_INIT *initid, UDF_ARGS *args, char *message)
{
    if (args->arg_count != 0)
        {
            strcpy(message,"Wrong arguments to my_getpid;  Use the source");
            return 1;
        }
    return 0;
}

void my_getpid_deinit(UDF_INIT *initid __attribute__((unused)))
{
    
}

const char* my_getpid(UDF_INIT *initid __attribute__((unused)),
    UDF_ARGS *args, char *result, unsigned long *length,
    char *is_null, char *error __attribute__((unused)))
{
    THD *thd = current_thd;
    return thd->proc_info; //getpid();
}

#endif
