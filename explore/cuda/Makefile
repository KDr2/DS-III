NVCC?=/usr/local/cuda/bin/nvcc

101.bin: 101.cu
	$(NVCC) -gencode arch=compute_60,code=sm_60 101.cu -o 101.bin


uva.bin: uva.cu
	$(NVCC) -gencode arch=compute_60,code=sm_60 uva.cu -o uva.bin

mat_mul.bin: mat_mul.cu
	$(NVCC) -gencode arch=compute_60,code=sm_60 mat_mul.cu -o mat_mul.bin
