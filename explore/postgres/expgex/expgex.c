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

void _PG_init(void);

void
_PG_init(void)
{
    elog(LOG, "EXPGEX:");
    elog(LOG, "\t INIT in process %d!", getpid());
    if(process_shared_preload_libraries_in_progress) {
        BackgroundWorker worker;

        memset(&worker, 0, sizeof(worker));
        worker.bgw_flags = BGWORKER_SHMEM_ACCESS |
            BGWORKER_BACKEND_DATABASE_CONNECTION;
        worker.bgw_start_time = BgWorkerStart_RecoveryFinished;
        worker.bgw_restart_time = BGW_NEVER_RESTART;
        // worker.bgw_main = &worker_spi_main;
        sprintf(worker.bgw_library_name, "/data/kdr2/mine/DS-III/explore/postgres/expgex/exworker.so");
        sprintf(worker.bgw_function_name, "worker_spi_main");
        worker.bgw_notify_pid = 0;

        snprintf(worker.bgw_name, BGW_MAXLEN, "worker %d", 0);
        worker.bgw_main_arg = Int32GetDatum(0);

        RegisterBackgroundWorker(&worker);

        elog(LOG, "\t if I am in shared_preload_libraries, I RUN ONLY IN MASTER PROCESS.");
    } else {
        elog(LOG, "\t if I am in local/session_preload_libraries, I RUN IN ALL REGULAR PROCESSES, includes startup process.");
    }
}


PG_FUNCTION_INFO_V1(add_one);


Datum
add_one(PG_FUNCTION_ARGS)
{
    int32 arg = PG_GETARG_INT32(0);
    PG_RETURN_INT32(arg + 1);
}
