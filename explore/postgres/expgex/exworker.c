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

#include "y/y.h"

#ifdef PG_MODULE_MAGIC
PG_MODULE_MAGIC;
#endif

int test_c0(void);
void worker_spi_main(Datum main_arg);

void
worker_spi_main(Datum main_arg)
{
    elog(LOG, "Run worker 0");
    TestGoFunc1(&test_c0);
    BackgroundWorkerUnblockSignals();

    Serve();
    elog(LOG, "Return from worker 0");
}

int test_c0(void)
{
    elog(LOG, "I'm in C.");
    return getpid();
}
