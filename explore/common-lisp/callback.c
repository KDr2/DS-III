#include <stdio.h>

typedef int (*CF)(int i, int j);

CF callback_func;

void set_cbf(CF p){
    callback_func = p;
}

void call_cbf(){
    printf("I'm from c: %d\n", (*callback_func)(2, 3));
}
