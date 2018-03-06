#include <stdio.h>

// Matrices are stored in row-major order:
// M(row, col) = *(M.elements + row * M.width + col)
typedef struct {
    int width;
    int height;
    float* elements;
} Matrix;


void print_matrix(const Matrix mat) {
    for(int r=0; r<mat.height; ++r) {
        for(int c=0; c<mat.width; ++c) {
            printf("%.2f\t", mat.elements[mat.width * r + c]);
        }
        printf("\n");
    }
}

// Thread block size
#define BLOCK_SIZE 16

// Forward declaration of the matrix multiplication kernel
__global__ void MatMulKernel(const Matrix, const Matrix, Matrix);

// Matrix multiplication - Host code
// Matrix dimensions are assumed to be multiples of BLOCK_SIZE

void MatMul(const Matrix A, const Matrix B, Matrix C) {
    // Load A and B to device memory
    Matrix d_A;
    d_A.width = A.width;
    d_A.height = A.height;
    size_t size = A.width * A.height * sizeof(float);
    cudaMalloc(&d_A.elements, size);
    cudaMemcpy(d_A.elements, A.elements, size, cudaMemcpyHostToDevice);

    Matrix d_B;
    d_B.width = B.width;
    d_B.height = B.height;
    size = B.width * B.height * sizeof(float);
    cudaMalloc(&d_B.elements, size);
    cudaMemcpy(d_B.elements, B.elements, size, cudaMemcpyHostToDevice);

    // Allocate C in device memory
    Matrix d_C;
    d_C.width = C.width;
    d_C.height = C.height;
    size = C.width * C.height * sizeof(float);
    cudaMalloc(&d_C.elements, size);

    // Invoke kernel
    dim3 dimBlock(BLOCK_SIZE, BLOCK_SIZE); // 16x16
    // result shape, e.g. mat_C.shape is (A.height x b.width)
    dim3 dimGrid((B.width + dimBlock.x -1) / dimBlock.x, (A.height + dimBlock.y -1) / dimBlock.y); // 2x2
    MatMulKernel<<<dimGrid, dimBlock>>>(d_A, d_B, d_C);

    // Read C from device memory
    cudaMemcpy(C.elements, d_C.elements, size, cudaMemcpyDeviceToHost);

    // Free device memory
    cudaFree(d_A.elements);
    cudaFree(d_B.elements);
    cudaFree(d_C.elements);
}

// Matrix multiplication kernel called by MatMul()
__global__ void MatMulKernel(Matrix A, Matrix B, Matrix C) {
    // Each thread computes one element of C
    // by accumulating results into Cvalue
    float Cvalue = 0;
    int row = blockIdx.y * blockDim.y + threadIdx.y;
    int col = blockIdx.x * blockDim.x + threadIdx.x;
    if(row >= C.height || col >= C.width) return; // this thread should not do any computing
    for (int e = 0; e < A.width; ++e)
        Cvalue += A.elements[row * A.width + e] * B.elements[e * B.width + col];
    C.elements[row * C.width + col] = Cvalue;
}


int main(int argc, char*argv[]) {
    Matrix mat_A;
    mat_A.height = 30;
    mat_A.width = 40;
    mat_A.elements = (float*)malloc(1200 * sizeof(float));
    for(int i=0; i<1200; ++i)mat_A.elements[i] = 1.0;

    Matrix mat_B;
    mat_B.height = 40;
    mat_B.width = 30;
    mat_B.elements = (float*)malloc(1200 * sizeof(float));
    for(int i=0; i<1200; ++i)mat_B.elements[i] = 1.0;

    Matrix mat_C;
    mat_C.width = 30;
    mat_C.height = 30;
    mat_C.elements = (float*)malloc(900 * sizeof(float));

    MatMul(mat_A, mat_B, mat_C);

    print_matrix(mat_C);
    return 0;
}