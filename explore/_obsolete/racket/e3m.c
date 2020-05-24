#include "scheme.h"
#include "module_1.rkt-mod.c"

static int run(Scheme_Env *e, int argc, char *argv[]) {

    Scheme_Object *l;
    Scheme_Object *a[2];

    MZ_GC_DECL_REG(6);
    MZ_GC_VAR_IN_REG(0, e);
    MZ_GC_VAR_IN_REG(1, l);
    MZ_GC_ARRAY_VAR_IN_REG(2, a, 2);
    MZ_GC_REG();

    declare_modules(e);

    l = scheme_make_null();
    l = scheme_make_pair(scheme_intern_symbol("module_1"), l);
    l = scheme_make_pair(scheme_intern_symbol("quote"), l);

    a[0] = l;
    a[1] = scheme_false;

    scheme_dynamic_require(2, a);

    MZ_GC_UNREG();

    return 0;
}



int main(int argc, char *argv[]){
    return scheme_main_setup(1, run, argc, argv);
}
