#include "future.h"

typedef void *(*function_t)(void *);

void run_future(void* future, size_t fut_size __attribute__((unused))) {
    future_t* fut = (future_t*)future;

    if(pthread_mutex_lock(&fut->lock) != 0)
        syserr("future pthread_mutex_lock");
    fut->res = fut->callable->function(fut->callable->arg, fut->callable->argsz, &fut->result_size);
    fut->result_to_obtain = 1;
    if(pthread_cond_broadcast(&fut->cond) != 0)
        syserr("future pthread_cond_broadcast");
    if(pthread_mutex_unlock(&fut->lock) != 0)
        syserr("future pthread_mutex_unlock");
}

void run_map(void *pair, size_t fut_pair_size __attribute__((unused))) {
    future_pair_t *future_pair = (future_pair_t*)pair;
    future_t *mapped_value = future_pair->mapped_value;
    future_t *future_value = future_pair->future_value;

    if(pthread_mutex_lock(&mapped_value->lock) != 0)
        syserr("future map pthread_mutex_lock");

    mapped_value->callable->arg = await(future_value);
    mapped_value->callable->argsz = future_value->result_size;
    mapped_value->res = mapped_value->callable->function(mapped_value->callable->arg, mapped_value->callable->argsz,
            &mapped_value->result_size);
    mapped_value->result_to_obtain = 1;
    if(pthread_cond_broadcast(&mapped_value->cond) != 0)
        syserr("future map pthread_cond_broadcast");
    free(pair);

    if(pthread_mutex_unlock(&mapped_value->lock) != 0)
        syserr("future map pthread_mutex_unlock");
}

int async(thread_pool_t *pool, future_t *future, callable_t callable) {
    future->callable = malloc(sizeof(callable_t));
    if(future->callable == NULL || pthread_mutex_init(&(future->lock), 0) != 0) {
        free(future->callable);
        return -1;
    }

    if(pthread_cond_init(&future->cond, NULL) != 0) {
        pthread_mutex_destroy(&future->lock);
        free(future->callable);
        return -1;
    }

    future->callable->argsz = callable.argsz;
    future->callable->arg = callable.arg;
    future->callable->function = callable.function;
    future->result_to_obtain = 0;

    if(defer(pool, (runnable_t){.function = run_future, .arg = future, .argsz = sizeof(future_t)}) != 0) {
        pthread_cond_destroy(&future->cond);
        pthread_mutex_destroy(&future->lock);
        free(future->callable);
        return -1;
    }

    return 0;
}

int map(thread_pool_t *pool, future_t *future, future_t *from, void *(*function)(void *, size_t, size_t *)) {

    future->callable = malloc(sizeof(callable_t));
    future_pair_t *pair = malloc(sizeof(future_pair_t));
    if(future->callable == NULL || pair == NULL || pthread_mutex_init(&future->lock, 0) != 0) {
        free(future->callable);
        return -1;
    }

    if(pthread_cond_init(&future->cond, NULL) != 0) {
        pthread_mutex_destroy(&future->lock);
        free(pair);
        free(future->callable);
        return -1;
    }

    future->callable->function = function;
    pair->mapped_value = future;
    pair->future_value = from;
    future->result_to_obtain = 0;

    if(defer(pool, (runnable_t){.function = run_map, .arg = pair, .argsz = sizeof(future_pair_t)}) != 0) {
        pthread_cond_destroy(&future->cond);
        pthread_mutex_destroy(&future->lock);
        free(future->callable);
        free(pair);
        return -1;
    }

    return 0;
}

void *await(future_t *future) {
    if(pthread_mutex_lock(&future->lock) != 0)
        syserr("await pthread_mutex_lock");
    while(!future->result_to_obtain)
        if(pthread_cond_wait(&future->cond, &future->lock) != 0)
            syserr("future pthread_cond_wait");
    if(pthread_mutex_unlock(&future->lock) != 0)
        syserr("await pthread_mutex_unlock");

    return future->res;
}

void future_destroy(future_t *future) {
    if(pthread_cond_destroy(&future->cond) != 0)
        syserr("future_destroy pthread_cond_destroy");
    if(pthread_mutex_destroy(&future->lock) != 0)
        syserr("future_destroy pthread_mutex_destroy");
    free(future->callable);
    future->res = NULL;
    future->result_to_obtain = 0;
    future->result_size = 0;
}