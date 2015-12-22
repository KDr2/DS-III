#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>
#include <wctype.h>
#include <sys/types.h>
#include <locale.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include "llt.h"
#include "flisp.h"
#include "opcodes.h"

static value_t argv_list(int argc, char *argv[])
{
    int i;
    value_t lst=FL_NIL, temp;
    fl_gc_handle(&lst);
    fl_gc_handle(&temp);
    for(i=argc-1; i >= 0; i--) {
        temp = cvalue_static_cstring(argv[i]);
        lst = fl_cons(temp, lst);
    }
    fl_free_gc_handles(2);
    return lst;
}

extern value_t fl_file(value_t *args, uint32_t nargs);

static char flisp_system_image[] = {
#include "flisp.boot.inc"
};

extern fltype_t *iostreamtype;

int main(int argc, char *argv[])
{
    fl_init(2*512*1024);
    value_t img = cvalue(iostreamtype, sizeof(ios_t));
    ios_t *pi = value2c(ios_t*, img);
    ios_static_buffer(pi, flisp_system_image, sizeof(flisp_system_image));
    
    if (fl_load_system_image(img)) {
        ios_puts("fatal error loading system image\n", ios_stderr);
        return 1;
    }

    FL_TRY_EXTERN {
        (void)fl_applyn(1, symbol_value(symbol("__start")),
            argv_list(argc, argv));
    }
    FL_CATCH_EXTERN {
        ios_puts("fatal error:\n", ios_stderr);
        fl_print(ios_stderr, fl_lasterror);
        ios_putc('\n', ios_stderr);
        return 1;
    }
    return 0;
}
