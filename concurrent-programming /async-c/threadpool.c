#include "threadpool.h"

run_list_t *get_task(thread_pool_t *pool) {
    run_list_t *task;

    task = pool->first_node;

    if(pool->first_node->next == NULL) {
        pool->first_node = NULL;
        pool->last_node  = NULL;
    } else {
        pool->first_node = pool->first_node->next;
        task->next = NULL;
    }

    return task;
}

void *task_handling(void *arg) {
    thread_pool_t *pool = arg;
    run_list_t *node;

    while(1) {
        if (sem_wait(pool->resource) != 0)
            syserr("thread from pool resource sem_wait");

        if(pthread_mutex_lock(&(pool->lock)) != 0)
            syserr("thread from pool pthread_mutex_lock");
        if (pool->stop)
            break;
        node = get_task(pool);
        pool->not_working -= 1;
        if(pthread_mutex_unlock(&(pool->lock)) != 0)
            syserr("thread from pool pthread_mutex_unlock");

        node->task.function(node->task.arg, node->task.argsz);
        free(node);

        if(pthread_mutex_lock(&(pool->lock)) != 0)
            syserr("thread from pool pthread_mutex_lock");
        if (pool->stop)
            break;
        pool->not_working += 1;
        if(pthread_mutex_unlock(&(pool->lock)) != 0)
            syserr("thread from pool pthread_mutex_unlock");
    }

    pool->active_threads--;
    if(pool->active_threads == 0) {
        if(sem_post(pool->finished) != 0)
            syserr("thread from pool finished sem_post");
    }
    if(pthread_mutex_unlock(&(pool->lock)) != 0)
        syserr("thread from pool pthread_mutex_unlock");

    return NULL;
}

int thread_pool_init(thread_pool_t *pool, size_t num_threads) {

    pool->resource = malloc(sizeof(sem_t));
    pool->finished = malloc(sizeof(sem_t));

    if(pool->finished == NULL || pool->resource == NULL || sem_init(pool->resource, 1, 0) != 0) {
        free(pool->finished);
        free(pool->resource);
        return -1;
    }

    if(sem_init(pool->finished, 1, 0) != 0) {
        sem_destroy(pool->resource);
        free(pool->finished);
        free(pool->resource);
        return -1;
    }

    if(pthread_mutex_init(&(pool->lock), 0) != 0) {
        sem_destroy(pool->finished);
        sem_destroy(pool->resource);
        free(pool->finished);
        free(pool->resource);
        return -1;
    }
    
    pool->first_node = NULL;
    pool->last_node  = NULL;
    pool->stop = 0;
    pool->not_working = 0;
    pool->active_threads = 0;

    pthread_attr_t attr;
    if(pthread_attr_init(&attr) != 0) {
        pthread_mutex_destroy(&(pool->lock));
        sem_destroy(pool->finished);
        sem_destroy(pool->resource);
        free(pool->finished);
        free(pool->resource);
    }
    if (pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED) != 0) {
        pthread_mutex_destroy(&(pool->lock));
        sem_destroy(pool->finished);
        sem_destroy(pool->resource);
        free(pool->finished);
        free(pool->resource);
    }

    pthread_t t[num_threads];
    for (size_t i = 0; i < num_threads; i++) {
        pool->not_working += 1;
        pool->active_threads += 1;
        if((pthread_create(&(t[i]), &attr, task_handling, pool)) != 0) {
            pool->not_working -= 1;
            pool->active_threads -= 1;
            thread_pool_destroy(pool);
            return -1;
        }
    }

    return 0;
}

void thread_pool_destroy(thread_pool_t *pool) {
    if(pthread_mutex_lock(&(pool->lock)) != 0)
        syserr("pthread_mutex_lock in thread_pool_destroy");
    run_list_t *node;
    run_list_t *node2;
    node = pool->first_node;
    while (node != NULL) {
        node2 = node->next;
        free(node);
        node = node2;
    }
    pool->stop = 1;
    size_t i = pool->not_working;
    if(pthread_mutex_unlock(&(pool->lock)) != 0)
        syserr("pthread_mutex_unlock in thread_pool_destroy");

    while(i > 0) {
        if(sem_post(pool->resource) != 0)
            syserr("sem_post resource in thread_pool_destroy");
        i--;
    }

    if(sem_wait(pool->finished) != 0) {
        syserr("sem_wait finished in thread_pool_destroy");
    }

    if(pthread_mutex_destroy(&(pool->lock)) != 0)
        syserr("pthread_mutex_destroy in thread_pool_destroy");
    if(sem_destroy(pool->resource) != 0)
        syserr("sem_destroy resource in thread_pool_destroy");
    if(sem_destroy(pool->finished) != 0)
        syserr("sem_destroy finished in thread_pool_destroy");
    free(pool->resource);
    free(pool->finished);
}

int defer(struct thread_pool *pool, runnable_t runnable) {
    run_list_t *node = malloc(sizeof(run_list_t));
    if(node == NULL) {
        free(node);
        return -1;
    }

    node->task = runnable;
    node->next = NULL;

    if(pthread_mutex_lock(&(pool->lock)) != 0) {
        free(node);
        return -1;
    }

    run_list_t *temp = pool->last_node;

    if(pool->first_node == NULL) {
        pool->first_node = node;
        pool->last_node  = pool->first_node;
    } else {
        pool->last_node->next = node;
        pool->last_node = node;
    }

    if(sem_post(pool->resource) != 0) {
        if(pool->first_node == pool->last_node) {
            pool->first_node = NULL;
            pool->last_node = NULL;
        } else {
            temp->next = NULL;
            pool->last_node = temp;
        }
        free(node);
        if(pthread_mutex_unlock(&(pool->lock)) != 0)
            syserr("pthread_mutex_unlock in defer");

        return -1;
    }
    if(pthread_mutex_unlock(&(pool->lock)) != 0)
        syserr("pthread_mutex_unlock in defer");

    return 0;
}