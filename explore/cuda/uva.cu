#include <stdlib.h>
#include <stdio.h>

#define N 10

__global__ void VecAssign(float* A, float *B) {
    int i = threadIdx.x;
    A[i] = 10.0 * i;
    B[i] = 20.0 * i; // bad: B is not alloced by CUDA
}


int main() {
    float *xA, *xB;
    cudaMallocHost(&xA, N * sizeof(float));
    printf("uva ptr=%p\n", xA);

    xB = (float*)malloc(sizeof(float) * N);
    printf("host ptr=%p\n", xB);

    printf("------------- set value on host...\n");
    for(int i=0; i<N; i++) {
        *(xA + i) = i * 1.0;
        *(xB + i) = i * 2.0;
        printf("uva  %d: %f\n", i, *(xA+i));
        printf("host %d: %f\n", i, *(xB+i));
    }

    printf("------------- call kernel...\n");
    VecAssign<<<1, N>>>(xA, xB);
    for(int i=0; i<N; i++) {
        printf("uva  %02d: %f\n", i, *(xA+i));
        printf("host %02d: %f\n", i, *(xB+i));
    }

    cudaFreeHost(xA);
    free(xB);

    getchar();
    return 0;
}
