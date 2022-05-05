#ifndef FUTURE_H
#define FUTURE_H

#include "threadpool.h"

typedef struct callable {
  void *(*function)(void *, size_t, size_t *);
  void *arg;
  size_t argsz;
} callable_t;

typedef struct future {
    void *res;
    pthread_mutex_t lock;
    pthread_cond_t cond;
    callable_t *callable;
    size_t result_size;
    int result_to_obtain;
} future_t;

typedef struct future_pair {
    future_t *future_value;
    future_t *mapped_value;
} future_pair_t;

int async(thread_pool_t *pool, future_t *future, callable_t callable);

int map(thread_pool_t *pool, future_t *future, future_t *from,
        void *(*function)(void *, size_t, size_t *));

void *await(future_t *future);

void future_destroy(future_t *future);

#endif
