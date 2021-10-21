//
// getconf LEVEL1_DCACHE_LINESIZE
// getconf -a|grep -i cache
//
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>


int32_t** alloc_data(int cahce_align){
    static int32_t *ptr[2];
    if(cahce_align) {
        long cache_line_size = sysconf(_SC_LEVEL1_DCACHE_SIZE);
        int8_t *p0 = malloc(cache_line_size * 2);
        ptr[0] = (int32_t*)p0;
        ptr[1] = (int32_t*)(p0 + cache_line_size);
    } else {
        int32_t *p0 = malloc(sizeof(int32_t)*2);
        ptr[0] = p0;
        ptr[1] = p0 + 1;
    }
    return ptr;
}

int sum_thread_1(int32_t *v)
{
    int s = 0;
    int i;
    for (i = 0; i < 1000000; ++i)
        s += *v;
    return s;
}
 
void inc_thread_2(int32_t *v)
{
    int i;
    for (i = 0; i < 1000000; ++i)
        ++(*v);
}

int main(int argc, char **argv)
{
    pthread_t tid;
    pthread_attr_t pattr;
    pthread_attr_init(&pattr);
    int32_t *i0, *i1;
    int32_t **p = alloc_data(argc > 1); // give a arg to make data align to cache-line-size
    i0 = p[0];
    i1 = p[1];
    pthread_create(&tid, &pattr, (void*(*)(void*))sum_thread_1, i0);
    inc_thread_2(i1);
    pthread_join(tid, NULL);
    return 0;
}
