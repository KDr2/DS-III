#include "postgres.h"

#include "stdio.h"
#include <sys/types.h>
#include <unistd.h>

/* These are always necessary for a bgworker */
#include "miscadmin.h"
#include "postmaster/bgworker.h"
#include "storage/ipc.h"
#include "storage/latch.h"
#include "storage/lwlock.h"
#include "storage/proc.h"
#include "storage/shmem.h"

/* these headers are used by this particular worker's code */
#include "access/xact.h"

#ifdef PG_MODULE_MAGIC
PG_MODULE_MAGIC;
#endif

void		_PG_init(void);

void
_PG_init(void)
{
    elog(LOG, "EXPGEX:");
    elog(LOG, "\t INIT in process %d!", getpid());
    sleep(3);
    if(process_shared_preload_libraries_in_progress) {
        elog(LOG, "\t if I am in shared_preload_libraries, I RUN ONLY IN MASTER PROCESS.");
    } else {
        elog(LOG, "\t if I am in local/session_preload_libraries, I RUN IN ALL REGULAR PROCESSES, includes startup process.");
    }
}


PG_FUNCTION_INFO_V1(add_one);

Datum
add_one(PG_FUNCTION_ARGS)
{
    int32   arg = PG_GETARG_INT32(0);

    PG_RETURN_INT32(arg + 1);
}
