/**
 * Niestety program wypisuje poprawnie tylko pierwszą sumę. Nie zdążyłem naprawić błędu.
**/

#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "threadpool.h"

int count = 0;
int k, n;
sem_t semaphore;

static void add_to_sum(void *arg, size_t argsz __attribute__((unused))) {
    int **x = (int**)arg;
    *x[2] += *x[0];
    usleep(*x[1]);
    count += 1;
    if(count == k*n) {
        sem_post(&semaphore);
    }
    free(arg);
}

int main() {
    int i, j, v, t;
    scanf("%d%d", &k, &n);

    int **matrix = (int **)malloc(k * sizeof(int *));
    int **t_values = (int **)malloc(k * sizeof(int *));
    for(i = 0; i < k; i++){
        matrix[i] = (int *)malloc(n * sizeof(int));
        t_values[i] = (int *)malloc(n * sizeof(int));
    }

    for(i = 0; i < n; i++) {
        scanf("%d%d", &v, &t);
        matrix[i/n][i%n] = v;
        t_values[i/n][i%n] = t;
    }

    sem_init(&semaphore, 1, 0);

    thread_pool_t pool;
    thread_pool_init(&pool, 4);

    int *row_sums = malloc(sizeof(int)*k);

    for(i = 0; i < k; i++) {
        for(j = 0; j < n; j++) {
            int **arg = malloc(sizeof(int *)*3);
            arg[0] = *(matrix+i)+j;
            arg[1] = *(t_values+i)+j;
            arg[2] = row_sums+i;
            defer(&pool, (runnable_t){.function = add_to_sum, .arg = arg, .argsz = sizeof(int *)*3});
        }
    }

    sem_wait(&semaphore);

    for(i = 0; i < k; i++) {
        printf("%d\n", row_sums[i]);
        free(matrix[i]);
        free(t_values[i]);
    }
    thread_pool_destroy(&pool);
    free(row_sums);
    free(matrix);
    free(t_values);

    return 0;
}
