#ifndef THREADPOOL_H
#define THREADPOOL_H

#include <pthread.h>
#include <semaphore.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "err.h"

typedef struct runnable {
    void (*function)(void *, size_t);
    void *arg;
    size_t argsz;
} runnable_t;

typedef struct runnable_list {
    runnable_t task;
    struct runnable_list *next;
} run_list_t;

typedef struct thread_pool {
    run_list_t *first_node;
    run_list_t *last_node;
    pthread_mutex_t lock;
    sem_t *finished;
    sem_t *resource;
    size_t not_working;
    size_t active_threads;
    int stop;
} thread_pool_t;

int thread_pool_init(thread_pool_t *pool, size_t pool_size);

void thread_pool_destroy(thread_pool_t *pool);

int defer(thread_pool_t *pool, runnable_t runnable);

#endif