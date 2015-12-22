// g++ -o reorder -O3 reorder.c -lpthread
// Run in X86 CPU (Intel/AMD)

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <semaphore.h>

#define MFENCE 1

sem_t sem_ac, sem_bc, sem_fin;

static volatile int A;
static volatile int B;
static volatile int C;

void *setAC(void *args) {
	for (;;) {
		sem_wait(&sem_ac);
		A = 1;
        #ifdef MFENCE
		__sync_synchronize();
        #endif
		C = B; // not atomic: mov A %eax, mov $eax C
		sem_post(&sem_fin);
	}
}

void *setBC(void *args) {
	for (;;) {
		sem_wait(&sem_bc);
		B = 1;
        #ifdef MFENCE
		__sync_synchronize();
        #endif
		C = A; // not atomic: mov A %eax, mov $eax C
		sem_post(&sem_fin);
	}
}

int main(int argc, char *argv[]) {
	pthread_t t1, t2;
	int got = 0, iters = 0;

	sem_init(&sem_ac, 0, 0);
	sem_init(&sem_bc, 0, 0);
	sem_init(&sem_fin, 0, 0);

	pthread_create(&t1, NULL, setAC, NULL);
	pthread_create(&t2, NULL, setBC, NULL);

	for (;; iters++) {
		A = B = C = 0;
		__sync_synchronize();
		sem_post(&sem_ac);
		sem_post(&sem_bc);
		sem_wait(&sem_fin);
		sem_wait(&sem_fin);
		if (C == 0) {
			printf("got %d reorders after %d iterations\n", ++got, iters);
		}
	}

	return 0;
}
