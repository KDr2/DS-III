
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "include/base64_example.h"

int base64_test(void)
{
    const char *c="abc\ndef";
    char *x;
    base64_encode_alloc(c,strlen(c),&x);
    printf("src : %s\nret : %s\n",c,x);
    size_t rs=0;
    char *rv;
    base64_decode_alloc(x,strlen(x),&rv,&rs);
    printf("org : %s\n",rv);
    free(x);
    free(rv);
    return 0;
}
