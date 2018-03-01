#include <stdlib.h>
#include <stdio.h>

#define N 10

__global__ void VecAdd(float* A, float* B, float* C) {
    int i = threadIdx.x;
    printf("tid: x=%d\n", i);
    C[i] = A[i] + B[i];
}


int main() {
    float A[N] = {0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0};
    float B[N] = {0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0};
    float *C = (float*)malloc(sizeof(float) * N);

    float *dA, *dB, *dC;
    cudaMalloc((void**)&dA, N * sizeof(float));
    cudaMalloc((void**)&dB, N * sizeof(float));
    cudaMalloc((void**)&dC, N * sizeof(float));

    cudaMemcpy(dA, A, N * sizeof(float), cudaMemcpyHostToDevice);
    cudaMemcpy(dB, B, N * sizeof(float), cudaMemcpyHostToDevice);
    // Kernel invocation with N threads
    VecAdd<<<1, N>>>(dA, dB, dC);

    cudaMemcpy(C, dC, N * sizeof(float), cudaMemcpyDeviceToHost);

    for(int i=0; i<N; i++) {
        printf("%d: %f\n", i, *(C+i));
    }

    free(C);

    return 0;
}
