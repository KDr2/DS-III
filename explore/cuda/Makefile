NVCC?=/usr/local/cuda/bin/nvcc

101.bin: 101.cu
	$(NVCC) -gencode arch=compute_60,code=sm_60 101.cu -o 101.bin
